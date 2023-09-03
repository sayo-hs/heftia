-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Effect.Handler.Heftia.Reader where

import Control.Effect.Class (type (~>))
import Control.Effect.Class.Machinery.HFunctor (HFunctor)
import Control.Effect.Class.Reader (AskS, LocalS (Local), ask, pattern AskS)
import Control.Heftia (interpret, reinterpret)
import Control.Heftia.Final (Hef)
import Data.Hefty.Sum (Sum)

interpretReader :: HFunctor (Sum es) => r -> Hef (LocalS r ': AskS r ': es) ~> Hef es
interpretReader r = interpretAsk r . elaborateReader

elaborateReader ::
    HFunctor (Sum es) =>
    Hef (LocalS r ': AskS r ': es) ~> Hef (AskS r ': es)
elaborateReader = interpret \(Local f a) ->
    ($ a) $ reinterpret \AskS -> f <$> ask

interpretAsk :: HFunctor (Sum es) => r -> Hef (AskS r ': es) ~> Hef es
interpretAsk r = interpret \AskS -> pure r
