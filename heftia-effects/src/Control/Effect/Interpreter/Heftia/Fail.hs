-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2024 Sayo Koyoneda
License     :  MPL-2.0 (see the LICENSE file)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable
-}
module Control.Effect.Interpreter.Heftia.Fail where

import Control.Effect (type (~>))
import Control.Monad.Hefty.Interpret (interpretRec)
import Control.Monad.Hefty.Types (Eff)
import Control.Monad.IO.Class (liftIO)
import Data.Effect.Fail (Fail (Fail))
import Data.Effect.OpenUnion.Internal.FO (type (<|))

runFailIO :: (IO <| r) => Eff eh (Fail ': r) ~> Eff eh r
runFailIO = interpretRec \(Fail s) -> liftIO $ fail s
