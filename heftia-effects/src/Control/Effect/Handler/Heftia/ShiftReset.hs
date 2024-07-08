-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TemplateHaskell #-}

module Control.Effect.Handler.Heftia.ShiftReset where

import Control.Arrow ((>>>))
import Control.Effect (type (<<:), type (~>))
import Control.Effect.Hefty (
    Eff,
    injectH,
    interpretKAllH_,
    interpretKH_,
    interpretRecH,
    raiseH,
    runEff,
 )
import Control.Freer (Freer)
import Control.Monad (void, (<=<), (>=>))
import Control.Monad.Freer (MonadFreer)
import Data.Effect (LiftIns)
import Data.Effect.HFunctor (HFunctor, hfmap)
import Data.Effect.TH (
    Default (def),
    makeEffect',
    makeEffectH,
    noDeriveHFunctor,
    noExtTemplate,
    (&),
 )
import Data.Hefty.Union (HFunctorUnion, HFunctorUnion_ (ForallHFunctor), Union ((|+:)))
import Data.Kind (Type)

data Shift (r :: Type) m a where
    Shift :: forall r m a. ((a -> m r) -> m r) -> Shift r m a

makeEffect' (def & noDeriveHFunctor) noExtTemplate [] [''Shift]

runShift ::
    (MonadFreer c fr, Union u, c (Eff u fr '[] ef), HFunctor (u '[])) =>
    Eff u fr '[Shift r] ef r ->
    Eff u fr '[] ef r
runShift =
    interpretKH_ pure \k ->
        let k' = raiseH . k
         in runShift . \case
                Shift f -> f k'

callCC :: forall r m a. (Shift r <<: m, Monad m) => ((a -> m r) -> m a) -> m a
callCC f = shift @r \k -> f (k >=> exit) >>= k

exit :: (Shift r <<: f, Applicative f) => r -> f a
exit r = shift \_ -> pure r

getCC :: (Shift r <<: m, Monad m) => m (m r)
getCC = callCC \exit' -> let a = exit' a in pure a

withShift ::
    ( MonadFreer c fr
    , Union u
    , c (Eff u fr '[] '[LiftIns (Eff u fr eh ef)])
    , c (Eff u fr eh ef)
    , HFunctor (u '[])
    ) =>
    Eff u fr '[Shift r] '[LiftIns (Eff u fr eh ef)] r ->
    Eff u fr eh ef r
withShift = runShift >>> runEff
{-# INLINE withShift #-}

data Shift_ m a where
    Shift_ :: (forall (r :: Type). (a -> m r) -> m r) -> Shift_ m a

makeEffect' (def & noDeriveHFunctor) noExtTemplate [] [''Shift_]

data Reset m (a :: Type) where
    Reset :: m a -> Reset m a

makeEffectH [''Reset]

runShift_ ::
    (MonadFreer c fr, Union u, c (Eff u fr eh ef), HFunctor (u eh)) =>
    Eff u fr (Shift_ ': eh) ef ~> Eff u fr eh ef
runShift_ =
    interpretKAllH_ pure \k ->
        (\(Shift_ f) -> runShift_ $ f $ raiseH . k)
            |+: (k <=< injectH . hfmap runShift_)

runReset ::
    (Freer c fr, HFunctorUnion u, ForallHFunctor u eh) =>
    Eff u fr (Reset ': eh) ef ~> Eff u fr eh ef
runReset = interpretRecH \(Reset a) -> a
{-# INLINE runReset #-}

getCC_ :: (Shift_ <<: m, Monad m) => m (m ())
getCC_ = shift_ \k -> let k' = k $ void k' in k'
