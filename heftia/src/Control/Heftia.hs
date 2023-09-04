{-# LANGUAGE QuantifiedConstraints #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Heftia where

import Control.Effect.Class (LiftIns, unliftIns, type (~>))
import Control.Effect.Class.Machinery.HFunctor (HFunctor, hfmap)
import Data.Hefty.Union (weakenSig, type (<:))

class (forall sig. HFunctor sig => c (h sig)) => Heftia c h | h -> c where
    {-# MINIMAL liftSig, interpretH #-}

    -- | Lift a /signature/ into a Heftia monad.
    liftSig :: HFunctor sig => sig (h sig) a -> h sig a

    interpretH :: (c m, HFunctor sig) => (sig m ~> m) -> h sig a -> m a

    -- | Translate /signature/s embedded in a Heftia monad.
    translateH ::
        (HFunctor sig, HFunctor sig') =>
        (sig (h sig') ~> sig' (h sig')) ->
        h sig a ->
        h sig' a
    translateH phi = interpretH $ liftSig . phi
    {-# INLINE translateH #-}

    reinterpretH :: HFunctor sig => (sig (h sig) ~> h sig) -> h sig a -> h sig a
    reinterpretH = interpretH
    {-# INLINE reinterpretH #-}

retract :: (Heftia c h, c m) => h (LiftIns m) a -> m a
retract = interpretH unliftIns

sendH :: (s <: t, Heftia c h, HFunctor s, HFunctor t) => s (h s) a -> h t a
sendH = liftSig . weakenSig . hfmap (translateH weakenSig)
