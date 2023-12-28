-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Effect.ExtensibleTree where

import Control.Effect.Free (EffectfulF)
import Control.Effect.Hefty (Effectful)
import Control.Monad.Freer.Tree (FreerTree)
import Data.Hefty.Extensible (ExtensibleUnion)

infixr 4 !!
infixr 3 !

type (!!) = Effectful ExtensibleUnion FreerTree
type (!) = EffectfulF ExtensibleUnion FreerTree
