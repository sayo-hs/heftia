-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2023-2024 Yamada Ryo
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable

Type operators for extensible effectful programs based on the tree-structured encoded free
applicative.

See "Control.Applicative.Free".
-}
module Control.Effect.ExtensibleTreeA where

import Control.Applicative.Free (Ap)
import Control.Effect (type (~>))
import Control.Effect.Free (EffF, EffectfulF)
import Control.Effect.Free qualified as F
import Control.Effect.Hefty (Eff, Effectful)
import Control.Effect.Hefty qualified as H
import Data.Effect (LiftIns)
import Data.Hefty.Extensible (ExtensibleUnion)

type eh !! ef = Effectful ExtensibleUnion Ap eh ef
type (!) ef = EffectfulF ExtensibleUnion Ap ef

infixr 5 !!
infixr 4 !

type ehs :!! efs = Eff ExtensibleUnion Ap ehs efs
type (:!) efs = EffF ExtensibleUnion Ap efs

infixr 4 :!!
infixr 3 :!

runEff :: Applicative f => '[] :!! '[LiftIns f] ~> f
runEff = H.runEff
{-# INLINE runEff #-}

runEffF :: Applicative f => (:!) '[LiftIns f] ~> f
runEffF = F.runEffF
{-# INLINE runEffF #-}
