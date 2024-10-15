-- SPDX-License-Identifier: MPL-2.0

module Control.Monad.Hefty.Concurrent.Stream where

import Control.Monad.Hefty (Eff, interpretBy, raiseAllH, (&), type (<<|))
import Control.Monad.Hefty.Concurrent.Parallel (Parallel, liftP2)
import Control.Monad.Hefty.Coroutine (Status (Continue, Done))
import Control.Monad.Hefty.Input (Input (Input))
import Control.Monad.Hefty.Output (Output (Output))
import Data.Bifunctor (Bifunctor, bimap)
import Data.These (These (That, These, This))

connect
    :: forall v a b eh ef
     . (Parallel <<| eh)
    => Eff '[] (Output v ': ef) a
    -> Eff '[] (Input v ': ef) b
    -> Eff eh ef (StreamStatus (Eff '[] ef) v a b)
connect a b =
    runStream
        (a & interpretBy (pure . Done) \(Output v) k -> pure $ Continue v k)
        (b & interpretBy (pure . Done) \Input k -> pure $ Continue () k)

runStream
    :: forall v a b eh ef
     . (Parallel <<| eh)
    => Eff '[] ef (Status (Eff '[] ef) v () a)
    -> Eff '[] ef (Status (Eff '[] ef) () v b)
    -> Eff eh ef (StreamStatus (Eff '[] ef) v a b)
runStream a b = do
    (a', b') <- liftP2 (,) (raiseAllH a) (raiseAllH b)

    case (a', b') of
        (Done x, Done y) -> pure $ Equilibrium x y
        (Done x, Continue () k) -> pure $ Overdemand x k
        (Continue v k, Done y) -> pure $ Oversupply v (k ()) y
        (Continue v resumeProducer, Continue () resumeConsumer) ->
            runStream (resumeProducer ()) (resumeConsumer v)

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
