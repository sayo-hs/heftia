{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

{- |
Copyright   :  (c) 2024 Sayo Koyoneda
License     :  MPL-2.0 (see the LICENSE file)
Maintainer  :  ymdfield@outlook.jp

Coroutine-based, composable, and resumable concurrent streams.
-}
module Control.Monad.Hefty.Concurrent.Stream (
    module Control.Monad.Hefty.Concurrent.Stream,
    module Control.Monad.Hefty.Input,
    module Control.Monad.Hefty.Output,
)
where

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
    type (~>),
 )
import Control.Monad.Hefty.Concurrent.Parallel (Parallel, liftP2)
import Control.Monad.Hefty.Input
import Control.Monad.Hefty.Output
import Control.Monad.Hefty.State (State, evalState, evalStateIORef, get'', put'')
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
    Unit
        :: forall i o ans eh ef
         . Eff eh (Input i ': Output o ': ef) ans
        -> Machinery eh ef ans i o
    Connect
        :: forall a b c ans eh ef
         . Machinery eh ef ans a b
        -> Machinery eh ef ans b c
        -> Machinery eh ef ans a c

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

    first
        :: forall b c d
         . Machinery '[] ef ans b c
        -> Machinery '[] ef ans (b, d) (c, d)
    first = \case
        Unit m -> Unit $ evalState (Left Seq.Empty) $ buffering m
        Connect a b -> Connect (first a) (first b)

    {-# INLINE arr #-}
    {-# INLINE first #-}

buffering
    :: forall b c d ans eh ef
     . Eff eh (Input b ': Output c ': ef) ans
    -> Eff eh (State (Either (Seq c) d) ': Input (b, d) ': Output (c, d) ': ef) ans
buffering =
    bundleN @2
        >>> reinterpret
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
        >>> unkey @"buffer"

instance ArrowChoice (Machinery '[] ef ans) where
    left = leftMachinery
    {-# INLINE left #-}

leftMachinery
    :: forall b c d ans eh ef
     . Machinery eh ef ans b c
    -> Machinery eh ef ans (Either b d) (Either c d)
leftMachinery = \case
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
    Connect a b -> Connect (leftMachinery a) (leftMachinery b)

newtype Machine f ans i o = Machine
    {runMachine :: f (MachineStatus f ans i o)}

data MachineStatus f ans i o
    = Terminated ans
    | Waiting (i -> Machine f ans i o)
    | Produced o (Machine f ans i o)

machine :: Eff '[] (Input i ': Output o ': ef) ans -> Machine (Eff eh ef) ans i o
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
    -> Eff eh ef (MachineStatus (Eff eh ef) ans i o)
runMachinery = runMachineryL . mviewl

runMachineryL
    :: forall i o ans eh ef
     . (Parallel <<| eh, Semigroup ans)
    => MachineryViewL '[] ef ans i o
    -> Eff eh ef (MachineStatus (Eff eh ef) ans i o)
runMachineryL = \case
    MOne m -> runMachine $ machine m
    MCons m ms -> do
        liftP2 (,) (runMachine $ machine m) (runMachinery ms) >>= loop
      where
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

newtype MachineryIO eh ef ans i o = MachineryIO {unMachineryIO :: Machinery eh ef ans i o}
    deriving newtype (Category)

instance (IO <| ef) => Arrow (MachineryIO eh ef ans) where
    arr (f :: b -> c) =
        MachineryIO . Unit . forever $
            input @b >>= output . f

    first :: forall b c d. MachineryIO eh ef ans b c -> MachineryIO eh ef ans (b, d) (c, d)
    first =
        unMachineryIO
            >>> MachineryIO . \case
                Unit m ->
                    Unit $ evalStateIORef (Left Seq.Empty) $ buffering m
                Connect a b ->
                    Connect
                        (unMachineryIO $ first $ MachineryIO a)
                        (unMachineryIO $ first $ MachineryIO b)

    {-# INLINE arr #-}
    {-# INLINE first #-}

instance (IO <| ef) => ArrowChoice (MachineryIO eh ef ans) where
    left = MachineryIO . leftMachinery . unMachineryIO
    {-# INLINE left #-}

runMachineryIO
    :: forall i o ans eh ef
     . (UnliftIO <<| eh, IO <| ef)
    => Eff eh ef i
    -> (o -> Eff eh ef ())
    -> Machinery eh ef ans i o
    -> Eff eh ef ans
runMachineryIO i o = runMachineryIOL i o . mviewl

runMachineryIOL
    :: forall i o ans eh ef
     . (UnliftIO <<| eh, IO <| ef)
    => Eff eh ef i
    -> (o -> Eff eh ef ())
    -> MachineryViewL eh ef ans i o
    -> Eff eh ef ans
runMachineryIOL i o = \case
    MOne m -> runUnit o m
    MCons a b ->
        withRunInIO \run -> do
            chan <- newEmptyTMVarIO
            ans <- newEmptyTMVarIO
            mask \restore -> do
                let runThread m = forkIO do
                        x <- restore $ run m
                        atomically $ putTMVar ans x

                t1 <- runThread $ runUnit (liftIO . atomically . putTMVar chan) a
                t2 <- runThread $ runMachineryIO (liftIO . atomically $ takeTMVar chan) o b

                atomically (readTMVar ans)
                    <* uninterruptibleMask_ (killThread t1 *> killThread t2)
  where
    runUnit :: (o' -> Eff eh ef ()) -> Eff eh (Input i ': Output o' ': ef) ~> Eff eh ef
    runUnit o' m =
        m
            & interpret (\Input -> raise i)
            & interpret (\(Output x) -> o' x)

runMachineryIO_
    :: forall ans eh ef
     . (UnliftIO <<| eh, IO <| ef)
    => Machinery eh ef ans () ()
    -> Eff eh ef ans
runMachineryIO_ = runMachineryIO (pure ()) (const $ pure ())
{-# INLINE runMachineryIO_ #-}

-- Inspired by https://hackage.haskell.org/package/freer-simple-1.2.1.2/docs/Data-FTCQueue.html

{- |
Left view deconstruction data structure for Machinery Pipeline.

This allows the number of generated threads to be reduced to the number of machine units.
-}
data MachineryViewL eh ef ans i o where
    MOne
        :: forall i o ans eh ef
         . Eff eh (Input i ': Output o ': ef) ans
        -> MachineryViewL eh ef ans i o
    MCons
        :: forall a b c ans eh ef
         . Eff eh (Input a ': Output b ': ef) ans
        -> Machinery eh ef ans b c
        -> MachineryViewL eh ef ans a c

-- | Left view deconstruction for Machinery Pipeline. [average O(1)]
mviewl :: Machinery eh ef ans i o -> MachineryViewL eh ef ans i o
mviewl = \case
    Unit m -> MOne m
    Connect a b -> connect a b
  where
    connect
        :: Machinery eh ef ans a b
        -> Machinery eh ef ans b c
        -> MachineryViewL eh ef ans a c
    connect (Unit m) r = m `MCons` r
    connect (Connect a b) r = connect a (Connect b r)
