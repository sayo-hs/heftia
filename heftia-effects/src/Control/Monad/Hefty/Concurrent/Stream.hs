{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

{- |
Copyright   :  (c) 2024 Sayo contributors
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
    Emb,
    FOEs,
    RemoveHOEs,
    WeakenHOEs,
    interpret,
    interpretsBy,
    nil,
    onlyFOEs,
    raise,
    reinterprets,
    untag,
    (!:),
    (&),
    (:>),
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

data Machinery es ans i o where
    Unit
        :: forall i o ans es
         . Eff (Input i ': Output o ': es) ans
        -> Machinery es ans i o
    Connect
        :: forall a b c ans es
         . Machinery es ans a b
        -> Machinery es ans b c
        -> Machinery es ans a c

instance Category (Machinery es ans) where
    id :: forall a. Machinery es ans a a
    id =
        Unit . forever $
            input @a >>= output

    (.) = flip Connect

    {-# INLINE id #-}
    {-# INLINE (.) #-}

instance (FOEs es) => Arrow (Machinery es ans) where
    arr (f :: b -> c) =
        Unit . forever $
            input @b >>= output . f

    first
        :: forall b c d
         . Machinery es ans b c
        -> Machinery es ans (b, d) (c, d)
    first = \case
        Unit m -> Unit $ evalState (Left Seq.Empty) $ buffering m
        Connect a b -> Connect (first a) (first b)

    {-# INLINE arr #-}
    {-# INLINE first #-}

buffering
    :: forall b c d ans es
     . Eff (Input b ': Output c ': es) ans
    -> Eff (State (Either (Seq c) d) ': Input (b, d) ': Output (c, d) ': es) ans
buffering =
    reinterprets
        ( ( \Input -> do
                (b, d) <- input

                get'' @"buffer" >>= \case
                    Right _ -> pure ()
                    Left outputQueue -> forM_ outputQueue \c -> output (c, d)

                put'' @"buffer" $ Right d

                pure b
          )
            !: ( \(Output c) ->
                    get'' @"buffer" >>= \case
                        Right d -> output (c, d)
                        Left outputQueue -> put'' @"buffer" $ Left $ outputQueue :|> c
               )
            !: nil
        )
        >>> untag @"buffer"

instance (FOEs es) => ArrowChoice (Machinery es ans) where
    left = leftMachinery
    {-# INLINE left #-}

leftMachinery
    :: forall b c d ans es
     . Machinery es ans b c
    -> Machinery es ans (Either b d) (Either c d)
leftMachinery = \case
    Unit m ->
        m
            & reinterprets
                ( ( \Input -> fix \next ->
                        input @(Either b d) >>= \case
                            Left x -> pure x
                            Right o -> do
                                output @(Either c d) $ Right o
                                next
                  )
                    !: (\(Output o) -> output @(Either c d) $ Left o)
                    !: nil
                )
            & Unit
    Connect a b -> Connect (leftMachinery a) (leftMachinery b)

newtype Machine f ans i o = Machine
    {runMachine :: f (MachineStatus f ans i o)}

data MachineStatus f ans i o
    = Terminated ans
    | Waiting (i -> Machine f ans i o)
    | Produced o (Machine f ans i o)

machine :: (WeakenHOEs es) => Eff (Input i ': Output o ': RemoveHOEs es) ans -> Machine (Eff es) ans i o
machine =
    interpretsBy
        (pure . Terminated)
        ( (\Input k -> pure $ Waiting $ Machine . onlyFOEs . k)
            !: (\(Output o) k -> pure $ Produced o $ Machine $ onlyFOEs $ k ())
            !: nil
        )
        >>> onlyFOEs
        >>> Machine

runMachinery
    :: forall i o ans es
     . (Parallel :> es, Semigroup ans, WeakenHOEs es)
    => Machinery (RemoveHOEs es) ans i o
    -> Eff es (MachineStatus (Eff es) ans i o)
runMachinery = runMachineryL . mviewl

runMachineryL
    :: forall i o ans es
     . (Parallel :> es, Semigroup ans, WeakenHOEs es)
    => MachineryViewL (RemoveHOEs es) ans i o
    -> Eff es (MachineStatus (Eff es) ans i o)
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

newtype MachineryIO es ans i o = MachineryIO {unMachineryIO :: Machinery es ans i o}
    deriving newtype (Category)

instance (Emb IO :> es) => Arrow (MachineryIO es ans) where
    arr (f :: b -> c) =
        MachineryIO . Unit . forever $
            input @b >>= output . f

    first :: forall b c d. MachineryIO es ans b c -> MachineryIO es ans (b, d) (c, d)
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

instance (Emb IO :> es) => ArrowChoice (MachineryIO es ans) where
    left = MachineryIO . leftMachinery . unMachineryIO
    {-# INLINE left #-}

runMachineryIO
    :: forall i o ans es
     . (UnliftIO :> es, Emb IO :> es)
    => Eff es i
    -> (o -> Eff es ())
    -> Machinery es ans i o
    -> Eff es ans
runMachineryIO i o = runMachineryIOL i o . mviewl

runMachineryIOL
    :: forall i o ans es
     . (UnliftIO :> es, Emb IO :> es)
    => Eff es i
    -> (o -> Eff es ())
    -> MachineryViewL es ans i o
    -> Eff es ans
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
    runUnit :: (o' -> Eff es ()) -> Eff (Input i ': Output o' ': es) ~> Eff es
    runUnit o' m =
        m
            & interpret (\Input -> raise i)
            & interpret (\(Output x) -> o' x)

runMachineryIO_
    :: forall ans es
     . (UnliftIO :> es, Emb IO :> es)
    => Machinery es ans () ()
    -> Eff es ans
runMachineryIO_ = runMachineryIO (pure ()) (const $ pure ())
{-# INLINE runMachineryIO_ #-}

-- Inspired by https://hackage.haskell.org/package/freer-simple-1.2.1.2/docs/Data-FTCQueue.html

{- |
Left view deconstruction data structure for Machinery Pipeline.

This allows the number of generated threads to be reduced to the number of machine units.
-}
data MachineryViewL es ans i o where
    MOne
        :: forall i o ans es
         . Eff (Input i ': Output o ': es) ans
        -> MachineryViewL es ans i o
    MCons
        :: forall a b c ans es
         . Eff (Input a ': Output b ': es) ans
        -> Machinery es ans b c
        -> MachineryViewL es ans a c

-- | Left view deconstruction for Machinery Pipeline. [average O(1)]
mviewl :: Machinery es ans i o -> MachineryViewL es ans i o
mviewl = \case
    Unit m -> MOne m
    Connect a b -> connect a b
  where
    connect
        :: Machinery es ans a b
        -> Machinery es ans b c
        -> MachineryViewL es ans a c
    connect (Unit m) r = m `MCons` r
    connect (Connect a b) r = connect a (Connect b r)
