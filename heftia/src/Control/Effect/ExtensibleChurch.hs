-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2023-2024 Yamada Ryo
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable

Type operators for extensible effectful programs based on the Church-encoded Freer monad.
-}
module Control.Effect.ExtensibleChurch where

import Control.Effect (type (~>))
import Control.Effect.Free (EffF, EffectfulF)
import Control.Effect.Free qualified as F
import Control.Effect.Hefty (Eff, Effectful)
import Control.Effect.Hefty qualified as H
import Control.Monad.Freer.Church (FreerChurch)
import Data.Effect (LiftIns)
import Data.Hefty.Extensible (ExtensibleUnion)

type eh !! ef = Effectful ExtensibleUnion FreerChurch eh ef
type (!) ef = EffectfulF ExtensibleUnion FreerChurch ef

infixr 5 !!
infixr 4 !

type ehs :!! efs = Eff ExtensibleUnion FreerChurch ehs efs
type (:!) efs = EffF ExtensibleUnion FreerChurch efs

infixr 4 :!!
infixr 3 :!

runEff :: Monad f => '[] :!! '[LiftIns f] ~> f
runEff = H.runEff
{-# INLINE runEff #-}

runEffF :: Monad f => (:!) '[LiftIns f] ~> f
runEffF = F.runEffF
{-# INLINE runEffF #-}
