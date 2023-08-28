{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Heftia where

import Control.Hefty (HFunctor, LiftIns, hmap, unliftIns)
import Control.Natural (type (~>))
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

send :: (s <: t, Heftia c h, HFunctor s, HFunctor t) => s (h s) a -> h t a
send = liftSig . weakenSig . hmap (translateH weakenSig)

retract :: (Heftia c h, c m) => h (LiftIns m) a -> m a
retract = interpretH unliftIns
