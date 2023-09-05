{-# LANGUAGE QuantifiedConstraints #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Monad.Trans.Freer where

import Control.Effect.Class (Instruction, type (~>))
import Control.Freer.Trans (TransFreer, hoistFreer, interpretFT, liftInsT, liftLower, transformT)
import Control.Monad.Cont (ContT)
import Control.Monad.Trans (MonadTrans, lift)
import Data.Coerce (Coercible, coerce)
import Data.Free.Sum (pattern L1, pattern R1, type (+))
import Data.Kind (Type)

class
    (TransFreer Monad fr, forall ins. MonadTrans (fr ins)) =>
    MonadTransFreer fr
    where
    interpretMK :: Monad m => (ins ~> ContT r m) -> fr ins m ~> ContT r m
    interpretMK = interpretMT
    {-# INLINE interpretMK #-}

    reinterpretMK :: Monad m => (ins ~> ContT r (fr ins m)) -> fr ins m ~> ContT r (fr ins m)
    reinterpretMK = reinterpretMT
    {-# INLINE reinterpretMK #-}

    interpretMT :: (Monad m, MonadTrans t, Monad (t m)) => (ins ~> t m) -> fr ins m ~> t m
    interpretMT = interpretFT lift
    {-# INLINE interpretMT #-}

    reinterpretMT ::
        forall m t n ins.
        (Monad m, MonadTrans t, Coercible n (fr ins m), Monad (t n), Monad n) =>
        (ins ~> t n) ->
        fr ins m ~> t n
    reinterpretMT f = interpretMT f . hoistFreer (coerce . liftLower @Monad @fr @ins)
    {-# INLINE reinterpretMT #-}

mergeFreer ::
    forall fr m ins ins' c.
    (TransFreer c fr, c m) =>
    fr ins (fr ins' m) ~> fr (ins + ins') m
mergeFreer = interpretFT (transformT @c R1) (liftInsT @c . L1)

splitFreer ::
    forall fr m ins ins' c.
    (TransFreer c fr, c m) =>
    fr (ins + ins') m ~> fr ins (fr ins' m)
splitFreer = interpretFT (liftLower . liftLower) \case
    L1 e -> liftInsT e
    R1 e -> liftLower $ liftInsT e

reinterpretTTViaFinal ::
    forall fr m t n ins.
    ( MonadTransFreer fr
    , Monad m
    , MonadTrans t
    , Coercible n (fr ins m)
    , Monad (t n)
    , Monad n
    ) =>
    (ins ~> t n) ->
    fr ins m ~> t n
reinterpretTTViaFinal = interpretFT $ lift . coerce . liftLower @Monad @fr @ins
{-# INLINE reinterpretTTViaFinal #-}

newtype ViaLiftLower (fr :: Instruction -> (Type -> Type) -> Type -> Type) ins m a = ViaLiftLower
    {runViaLiftLower :: fr ins m a}
    deriving newtype (Functor, Applicative, Monad)
    deriving stock (Foldable, Traversable)

instance TransFreer Monad h => MonadTrans (ViaLiftLower h ins) where
    lift = ViaLiftLower . liftLower
    {-# INLINE lift #-}
