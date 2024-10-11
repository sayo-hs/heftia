-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2024 Sayo Koyoneda
License     :  MPL-2.0 (see the LICENSE file)
Maintainer  :  ymdfield@outlook.jp
Portability :  portable
-}
module Control.Monad.Hefty.Fail (
    module Control.Monad.Hefty.Fail,
    module Data.Effect.Fail,
)
where

import Control.Monad.Fail qualified as IO
import Control.Monad.Hefty (Eff, interpret, liftIO, type (<|), type (~>))
import Data.Effect.Fail

runFailIO :: (IO <| ef) => Eff eh (Fail ': ef) ~> Eff eh ef
runFailIO = interpret \(Fail s) -> liftIO $ IO.fail s
