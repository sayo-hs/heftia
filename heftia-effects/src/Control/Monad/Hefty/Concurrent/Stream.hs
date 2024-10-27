{-# LANGUAGE ApplicativeDo #-}

-- SPDX-License-Identifier: MPL-2.0

module Control.Monad.Hefty.Concurrent.Stream where

import Control.Arrow (Arrow, ArrowChoice, arr, first, left, (>>>))
import Control.Category (Category)
import Control.Category qualified as C
import Control.Monad (forM_, forever)
import Control.Monad.Hefty (
    Eff,
    bundleN,
    interpret,
    interpretBy,
    nil,
    raise,
    raiseAllH,
    reinterpret,
    unkey,
    (!+),
    (&),
    type (<<|),
    type (<|),
 )
import Control.Monad.Hefty.Concurrent.Parallel (Parallel, liftP2)
import Control.Monad.Hefty.Input (Input (Input))
import Control.Monad.Hefty.Output (Output (Output))
import Control.Monad.Hefty.State (evalState, get'', put'')
import Data.Effect.Input (input)
import Data.Effect.Output (output)
import Data.Effect.Unlift (UnliftIO, withRunInIO)
import Data.Function (fix)
import Data.Sequence (Seq ((:|>)))
import Data.Sequence qualified as Seq
import UnliftIO (
    atomically,
    liftIO,
    mask,
    newEmptyTMVarIO,
    putTMVar,
    readTMVar,
    takeTMVar,
    uninterruptibleMask_,
 )
import UnliftIO.Concurrent (forkIO, killThread)

data Machinery eh ef ans i o where
    Unit :: forall i o ans eh ef. Eff eh (Input i ': Output o ': ef) ans -> Machinery eh ef ans i o
    Connect :: forall a b c ans eh ef. Machinery eh ef ans a b -> Machinery eh ef ans b c -> Machinery eh ef ans a c

instance Category (Machinery eh ef ans) where
    id :: forall a. Machinery eh ef ans a a
    id =
        Unit . forever $
            input @a >>= output

    (.) = flip Connect

    {-# INLINE id #-}
    {-# INLINE (.) #-}

instance Arrow (Machinery '[] ef ans) where
    arr (f :: b -> c) =
        Unit . forever $
            input @b >>= output . f

    first :: forall b c d. Machinery '[] ef ans b c -> Machinery '[] ef ans (b, d) (c, d)
    first = \case
        Unit m ->
            Unit $
                evalState (Left @(Seq c) @d Seq.Empty) . unkey @"buffer" $
                    bundleN @2 m
                        & reinterpret
                            ( ( \Input -> do
                                    (b, d) <- input

                                    get'' @"buffer" >>= \case
                                        Right _ -> pure ()
                                        Left outputQueue -> forM_ outputQueue \c -> output (c, d)

                                    put'' @"buffer" $ Right d

                                    pure b
                              )
                                !+ ( \(Output c) ->
                                        get'' @"buffer" >>= \case
                                            Right d -> output (c, d)
                                            Left outputQueue -> put'' @"buffer" $ Left $ outputQueue :|> c
                                   )
                                !+ nil
                            )
        Connect a b -> Connect (first a) (first b)

    {-# INLINE arr #-}
    {-# INLINE first #-}

instance ArrowChoice (Machinery '[] ef ans) where
    left :: forall b c d. Machinery '[] ef ans b c -> Machinery '[] ef ans (Either b d) (Either c d)
    left = \case
        Unit m ->
            bundleN @2 m
                & reinterpret
                    ( ( \Input -> fix \next ->
                            input @(Either b d) >>= \case
                                Left x -> pure x
                                Right o -> do
                                    output @(Either c d) $ Right o
                                    next
                      )
                        !+ (\(Output o) -> output @(Either c d) $ Left o)
                        !+ nil
                    )
                & Unit
        Connect a b -> Connect (left a) (left b)
    {-# INLINE left #-}

newtype Machine eh ef ans i o = Machine
    {runMachine :: Eff eh ef (MachineStatus eh ef ans i o)}

data MachineStatus eh ef ans i o
    = Terminated ans
    | Waiting (i -> Machine eh ef ans i o)
    | Produced o (Machine eh ef ans i o)

machine :: Eff '[] (Input i ': Output o ': ef) ans -> Machine eh ef ans i o
machine =
    bundleN @2
        >>> interpretBy
            (pure . Terminated)
            ( (\Input k -> pure $ Waiting $ Machine . raiseAllH . k)
                !+ (\(Output o) k -> pure $ Produced o $ Machine $ raiseAllH $ k ())
                !+ nil
            )
        >>> raiseAllH
        >>> Machine

runMachinery
    :: forall i o ans eh ef
     . (Parallel <<| eh, Semigroup ans)
    => Machinery '[] ef ans i o
    -> Eff eh ef (MachineStatus eh ef ans i o)
runMachinery = \case
    Unit m -> runMachine $ machine m
    Connect a b -> do
        liftP2 (,) (runMachinery a) (runMachinery b) >>= loop
      where
        loop
            :: (MachineStatus eh ef ans a b, MachineStatus eh ef ans b c)
            -> Eff eh ef (MachineStatus eh ef ans a c)
        loop = \case
            (Terminated ans, Terminated ans') -> pure $ Terminated $ ans <> ans'
            (Produced o k1, Waiting k2) ->
                liftP2 (,) (runMachine k1) (runMachine $ k2 o) >>= loop
            (Waiting k, s) ->
                pure $ Waiting \i -> Machine do
                    s' <- runMachine $ k i
                    loop (s', s)
            (s, Produced o k) ->
                pure $ Produced o $ Machine do
                    s' <- runMachine k
                    loop (s, s')
            (Terminated ans, Waiting _) -> pure $ Terminated ans
            (Produced _ _, Terminated ans) -> pure $ Terminated ans

runMachineryIO
    :: forall i o ans eh ef
     . (UnliftIO <<| eh, IO <| ef)
    => Eff eh ef i
    -> (o -> Eff eh ef ())
    -> Machinery eh ef ans i o
    -> Eff eh ef ans
runMachineryIO i o = \case
    Unit m ->
        m
            & interpret (\Input -> raise i)
            & interpret (\(Output x) -> o x)
    Connect a b ->
        withRunInIO \run -> do
            chan <- newEmptyTMVarIO
            ans <- newEmptyTMVarIO
            mask \restore -> do
                let runThread m = forkIO do
                        x <- restore $ run m
                        atomically $ putTMVar ans x

                t1 <- runThread $ runMachineryIO i (liftIO . atomically . putTMVar chan) a
                t2 <- runThread $ runMachineryIO (liftIO . atomically $ takeTMVar chan) o b

                atomically (readTMVar ans)
                    <* uninterruptibleMask_ (killThread t1 *> killThread t2)
