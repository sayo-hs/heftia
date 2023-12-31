{-# LANGUAGE QuantifiedConstraints #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2023 Yamada Ryo
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable

A type class to abstract away the encoding details of the Heftia carriers.
-}
module Control.Heftia where

import Control.Effect.Class (LiftIns, unliftIns, type (~>))
import Control.Effect.Class.Machinery.HFunctor (HFunctor)

-- | A type class to abstract away the encoding details of the Heftia carrier.
class (forall sig. HFunctor sig => c (h sig)) => Heftia c h | h -> c where
    {-# MINIMAL liftSig, interpretHH #-}

    -- | Lift a /signature/ into a Heftia carrier.
    liftSig :: HFunctor sig => sig (h sig) a -> h sig a

    interpretHH :: (c m, HFunctor sig) => (sig m ~> m) -> h sig a -> m a

    -- | Translate /signature/s embedded in a Heftia carrier.
    translateHH ::
        (HFunctor sig, HFunctor sig') =>
        (sig (h sig') ~> sig' (h sig')) ->
        h sig a ->
        h sig' a
    translateHH phi = interpretHH $ liftSig . phi
    {-# INLINE translateHH #-}

    reinterpretHH :: HFunctor sig => (sig (h sig) ~> h sig) -> h sig a -> h sig a
    reinterpretHH = interpretHH
    {-# INLINE reinterpretHH #-}

retractH :: (Heftia c h, c m) => h (LiftIns m) a -> m a
retractH = interpretHH unliftIns
{-# INLINE retractH #-}
