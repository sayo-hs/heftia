-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2024 Yamada Ryo
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable
-}
module Control.Effect.Handler.Heftia.Fail where

import Control.Effect (sendIns, type (~>))
import Control.Effect.Hefty (Eff, interpret)
import Control.Freer (Freer)
import Data.Effect.Fail (Fail (Fail), LFail)
import Data.Effect.HFunctor (HFunctor)
import Data.Hefty.Union (Member, Union)

runFailAsIO ::
    forall r fr u c.
    (Freer c fr, Union u, HFunctor (u '[]), Member u IO r) =>
    Eff u fr '[] (LFail ': r) ~> Eff u fr '[] r
runFailAsIO = interpret \(Fail s) -> sendIns @IO $ fail s
{-# INLINE runFailAsIO #-}
