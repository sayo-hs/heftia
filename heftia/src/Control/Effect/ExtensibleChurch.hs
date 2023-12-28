-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Effect.ExtensibleChurch where

import Control.Effect.Free (EffectfulF)
import Control.Effect.Hefty (Effectful)
import Control.Monad.Freer.Church (FreerChurch)
import Data.Hefty.Extensible (ExtensibleUnion)

infixr 4 !!
infixr 3 !

type (!!) = Effectful ExtensibleUnion FreerChurch
type (!) = EffectfulF ExtensibleUnion FreerChurch
