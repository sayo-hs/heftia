{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Heftia.Final where

import Control.Applicative (Alternative, empty, (<|>))
import Control.Effect.Class (Signature, type (~>))
import Control.Effect.Class.Machinery.HFunctor (HFunctor, hfmap)
import Control.Heftia (Heftia, HeftiaEffects, interpretH, liftSig)
import Control.Monad (MonadPlus (mplus, mzero))
import Data.Hefty.Sum (SumUnion)

newtype HeftiaFinal c (h :: Signature) a = HeftiaFinal
    {unHeftiaFinal :: forall f. c f => (h f ~> f) -> f a}

runHeftiaFinal :: c f => (h f ~> f) -> HeftiaFinal c h a -> f a
runHeftiaFinal i (HeftiaFinal f) = f i

liftSigFinal :: HFunctor h => h (HeftiaFinal c h) a -> HeftiaFinal c h a
liftSigFinal e = HeftiaFinal \i -> i $ hfmap (runHeftiaFinal i) e

weakenHeftiaFinal :: (forall f. c' f => c f) => HeftiaFinal c h a -> HeftiaFinal c' h a
weakenHeftiaFinal (HeftiaFinal f) = HeftiaFinal f

transformHeftiaFinal ::
    (forall f. h f ~> i f) ->
    HeftiaFinal c h a ->
    HeftiaFinal c i a
transformHeftiaFinal phi (HeftiaFinal f) = HeftiaFinal \i -> f $ i . phi

translateHeftiaFinal ::
    (c (HeftiaFinal c i), HFunctor i) =>
    (h (HeftiaFinal c i) ~> i (HeftiaFinal c i)) ->
    HeftiaFinal c h a ->
    HeftiaFinal c i a
translateHeftiaFinal f = runHeftiaFinal $ liftSigFinal . f

class Noop f
instance Noop f

instance (forall f. c f => Functor f, c (HeftiaFinal c h)) => Functor (HeftiaFinal c h) where
    fmap f (HeftiaFinal g) =
        HeftiaFinal \(i :: h f ~> f) -> f <$> g i

instance
    (forall f. c f => Applicative f, c (HeftiaFinal c h)) =>
    Applicative (HeftiaFinal c h)
    where
    pure x = HeftiaFinal \(_ :: h f ~> f) -> pure x

    HeftiaFinal f <*> HeftiaFinal g =
        HeftiaFinal \(i :: h f ~> f) -> f i <*> g i

instance
    (forall f. c f => Alternative f, c (HeftiaFinal c h)) =>
    Alternative (HeftiaFinal c h)
    where
    empty = HeftiaFinal \(_ :: h f ~> f) -> empty

    HeftiaFinal f <|> HeftiaFinal g =
        HeftiaFinal \(i :: h f ~> f) -> f i <|> g i

instance (forall m. c m => Monad m, c (HeftiaFinal c h)) => Monad (HeftiaFinal c h) where
    HeftiaFinal f >>= k =
        HeftiaFinal \(i :: h m ~> m) ->
            f i >>= runHeftiaFinal i . k

instance
    (forall m. c m => MonadPlus m, Alternative (HeftiaFinal c h), Monad (HeftiaFinal c h)) =>
    MonadPlus (HeftiaFinal c h)
    where
    mzero = HeftiaFinal \(_ :: h m ~> m) -> mzero

    HeftiaFinal f `mplus` HeftiaFinal g =
        HeftiaFinal \(i :: h m ~> m) -> f i `mplus` g i

instance (forall sig. c (HeftiaFinal c sig)) => Heftia c (HeftiaFinal c) where
    liftSig = liftSigFinal
    interpretH = runHeftiaFinal

type Hef es = HeftiaEffects (HeftiaFinal Monad) SumUnion es
type HefA es = HeftiaEffects (HeftiaFinal Applicative) SumUnion es
