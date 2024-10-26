{-# LANGUAGE ApplicativeDo #-}

-- SPDX-License-Identifier: MPL-2.0

module Control.Monad.Hefty.Concurrent.Stream where

import Control.Arrow (Arrow, ArrowChoice, arr, first, left, (>>>))
import Control.Category (Category)
import Control.Category qualified as C
import Control.Monad (forever)
import Control.Monad.Hefty (
    Eff,
    bundleN,
    interpretBy,
    nil,
    raiseAllH,
    unkey,
    untag,
    (!+),
    (&),
    type (#),
    type (#>),
    type (<<|),
    type (~>),
 )
import Control.Monad.Hefty.Concurrent.Parallel (Parallel, liftP2)
import Control.Monad.Hefty.Coroutine (Status (Continue, Done))
import Control.Monad.Hefty.Input (Input (Input))
import Control.Monad.Hefty.Output (Output (Output))
import Data.Bifunctor (Bifunctor, bimap)
import Data.Effect.Input (input)
import Data.Effect.Output (output)
import Data.Function (fix)
import Data.Sequence (Seq ((:|>)))
import Data.Sequence qualified as Seq
import Data.These (These (That, These, This))

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

data Machinery eh ef ans i o where
    MachineUnit :: Machine eh ef ans i o -> Machinery eh ef ans i o
    Connect :: Machinery eh ef ans a b -> Machinery eh ef ans b c -> Machinery eh ef ans a c

instance Category (Machinery eh ef ans) where
    id :: forall a. Machinery eh ef ans a a
    id =
        MachineUnit . machine . forever $
            input @a >>= output

    (.) = flip Connect

    {-# INLINE id #-}
    {-# INLINE (.) #-}

instance Arrow (Machinery eh ef ans) where
    arr (f :: b -> c) =
        MachineUnit . machine . forever $
            input @b >>= output . f

    first :: forall b c d. Machinery eh ef ans b c -> Machinery eh ef ans (b, d) (c, d)
    first = \case
        MachineUnit (Machine m) ->
            MachineUnit . Machine $
                loop (Left Seq.Empty) m
          where
            loop
                :: Either (Seq c) d
                -> Eff eh ef (MachineStatus eh ef ans b c)
                -> Eff eh ef (MachineStatus eh ef ans (b, d) (c, d))
            loop bufferState m' =
                m' >>= \case
                    Terminated ans -> pure $ Terminated ans
                    Waiting k ->
                        pure $ Waiting \(b, d) ->
                            let k' = loop (Right d) (runMachine $ k b)
                             in Machine
                                    case bufferState of
                                        Right _ -> k'
                                        Left outputQueue ->
                                            foldr (\c -> pure . Produced (c, d) . Machine) k' outputQueue
                    Produced c (Machine k) ->
                        case bufferState of
                            Right bufferedPassthrough ->
                                pure . Produced (c, bufferedPassthrough) . Machine $
                                    loop bufferState k
                            Left outputQueue ->
                                loop (Left $ outputQueue :|> c) k
        Connect a b -> Connect (first a) (first b)

    {-# INLINE arr #-}
    {-# INLINE first #-}

instance ArrowChoice (Machinery eh ef ans) where
    left = \case
        MachineUnit (Machine m) ->
            MachineUnit . Machine $ loop m
          where
            loop
                :: Eff eh ef (MachineStatus eh ef ans b c)
                -> Eff eh ef (MachineStatus eh ef ans (Either b d) (Either c d))
            loop m' =
                m' >>= \case
                    Terminated ans -> pure $ Terminated ans
                    Waiting k ->
                        fix \next ->
                            pure . Waiting $
                                Machine . \case
                                    Left b -> loop $ runMachine $ k b
                                    Right d -> pure $ Produced (Right d) (Machine next)
                    Produced c (Machine k) -> pure $ Produced (Left c) (Machine $ loop k)
        Connect a b -> Connect (left a) (left b)
    {-# INLINE left #-}

runMachinery
    :: forall i o ans eh ef
     . (Parallel <<| eh, Semigroup ans)
    => Machinery eh ef ans i o
    -> Eff eh ef (MachineStatus eh ef ans i o)
runMachinery = \case
    MachineUnit (Machine m) -> m
    Connect a b -> do
        liftP2 (,) (runMachinery a) (runMachinery b) >>= loop
      where
        loop
            :: (MachineStatus eh ef ans a b, MachineStatus eh ef ans b c)
            -> Eff eh ef (MachineStatus eh ef ans a c)
        loop = \case
            (Terminated ans, Terminated ans') -> pure $ Terminated $ ans <> ans'
            (Terminated ans, _) -> pure $ Terminated ans
            (_, Terminated ans) -> pure $ Terminated ans
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

source :: Eff '[] (Output v ': ef) a -> Eff '[] ef (Status (Eff '[] ef) v () a)
source = interpretBy (pure . Done) \(Output v) k -> pure $ Continue v k

sink :: Eff '[] (Input v ': ef) a -> Eff '[] ef (Status (Eff '[] ef) () v a)
sink = interpretBy (pure . Done) \Input k -> pure $ Continue () k

newtype Source eh ef ans o = Source {runSource :: Eff '[] ef (Status (Eff '[] ef) () o ans)}
newtype Sink eh ef ans i = Sink {runSink :: Eff '[] ef (Status (Eff '[] ef) i () ans)}

connect
    :: forall v a b eh ef
     . (Parallel <<| eh)
    => Eff '[] (Output v ': ef) a
    -> Eff '[] (Input v ': ef) b
    -> Eff eh ef (StreamStatus (Eff '[] ef) v a b)
connect a b =
    runStream
        raiseAllH
        (a & interpretBy (pure . Done) \(Output v) k -> pure $ Continue v k)
        (b & interpretBy (pure . Done) \Input k -> pure $ Continue () k)

connect'
    :: forall o i v a b eh ef
     . (Parallel <<| eh)
    => Eff '[] (Output v # o ': ef) a
    -> Eff '[] (Input v # i ': ef) b
    -> Eff eh ef (StreamStatus (Eff '[] ef) v a b)
connect' a b = connect (untag a) (untag b)
{-# INLINE connect' #-}

connect''
    :: forall o i v a b eh ef
     . (Parallel <<| eh)
    => Eff '[] (o #> Output v ': ef) a
    -> Eff '[] (i #> Input v ': ef) b
    -> Eff eh ef (StreamStatus (Eff '[] ef) v a b)
connect'' a b = connect (unkey a) (unkey b)
{-# INLINE connect'' #-}

runStream
    :: forall v a b eh eh' ef
     . (Parallel <<| eh')
    => (Eff eh ef ~> Eff eh' ef)
    -> Eff eh ef (Status (Eff eh ef) v () a)
    -> Eff eh ef (Status (Eff eh ef) () v b)
    -> Eff eh' ef (StreamStatus (Eff eh ef) v a b)
runStream f a b = do
    (a', b') <- liftP2 (,) (f a) (f b)

    case (a', b') of
        (Done x, Done y) -> pure $ Equilibrium x y
        (Done x, Continue () k) -> pure $ Overdemand x k
        (Continue v k, Done y) -> pure $ Oversupply v (k ()) y
        (Continue v resumeProducer, Continue () resumeConsumer) ->
            runStream f (resumeProducer ()) (resumeConsumer v)

data StreamStatus f v a b
    = Equilibrium a b
    | Overdemand a (v -> f (Status f () v b))
    | Oversupply v (f (Status f v () a)) b
    deriving (Functor)

instance (Functor f) => Bifunctor (StreamStatus f v) where
    bimap f g = \case
        Equilibrium x y -> Equilibrium (f x) (g y)
        Overdemand x k -> Overdemand (f x) ((fmap . fmap . fmap) g k)
        Oversupply v k y -> Oversupply v ((fmap . fmap) f k) (g y)

closing :: StreamStatus f v a b -> These a b
closing = \case
    Equilibrium x y -> These x y
    Overdemand x _ -> This x
    Oversupply _ _ y -> That y
