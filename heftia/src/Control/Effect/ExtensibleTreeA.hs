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
import Control.Effect.Free (EffectfulF)
import Control.Effect.Hefty (Effectful)
import Data.Hefty.Extensible (ExtensibleUnion)

infixr 4 !!
infixr 3 !

type (!!) = Effectful ExtensibleUnion Ap
type (!) = EffectfulF ExtensibleUnion Ap
