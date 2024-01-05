-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2023-2024 Yamada Ryo
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable

Type operators for extensible effectful programs based on the fast-encoded free applicative.

See "Control.Applicative.Free.Fast".
-}
module Control.Effect.ExtensibleFastA where

import Control.Applicative.Free.Fast (Ap)
import Control.Effect.Free (EffectfulF)
import Control.Effect.Hefty (Effectful)
import Data.Hefty.Extensible (ExtensibleUnion)

infixr 5 !!
infixr 4 !

type (!!) = Effectful ExtensibleUnion Ap
type (!) = EffectfulF ExtensibleUnion Ap
