-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2023-2024 Yamada Ryo
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable

Type operators for extensible effectful programs based on the tree-structured encoded Freer monad.
-}
module Control.Effect.ExtensibleTree where

import Control.Effect (type (~>))
import Control.Effect.Free (EffF, EffectfulF)
import Control.Effect.Free qualified as F
import Control.Effect.Hefty (Eff, Effectful)
import Control.Effect.Hefty qualified as H
import Control.Monad.Freer.Tree (FreerTree)
import Data.Effect (LiftIns)
import Data.Hefty.Extensible (ExtensibleUnion)

type eh !! ef = Effectful ExtensibleUnion FreerTree eh ef
type (!) ef = EffectfulF ExtensibleUnion FreerTree ef

infixr 5 !!
infixr 4 !

type ehs :!! efs = Eff ExtensibleUnion FreerTree ehs efs
type (:!) efs = EffF ExtensibleUnion FreerTree efs

infixr 5 :!!
infixr 4 :!

runEff :: Monad f => '[] :!! '[LiftIns f] ~> f
runEff = H.runEff
{-# INLINE runEff #-}

runEffF :: Monad f => (:!) '[LiftIns f] ~> f
runEffF = F.runEffF
{-# INLINE runEffF #-}
