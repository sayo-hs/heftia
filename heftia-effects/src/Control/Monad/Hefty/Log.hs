-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2024 Sayo Koyoneda
License     :  MPL-2.0 (see the LICENSE file)
Maintainer  :  ymdfield@outlook.jp

Interpreters for the [@co-log@](https://hackage.haskell.org/package/co-log) ecosystem.

The interface is similar to [@co-log-polysemy@](https://hackage.haskell.org/package/co-log-polysemy).
-}
module Control.Monad.Hefty.Log (
    module Control.Monad.Hefty.Log,
    module Data.Effect.Log,
)
where

import Colog.Core (LogAction (LogAction))
import Control.Monad.Hefty (Eff, interpret, send, type (<|), type (~>))
import Control.Monad.Hefty.Output (Output (..), output)
import Data.Effect.Log
import Prelude hiding (log)
