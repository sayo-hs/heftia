-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Effect.Handler.Heftia.Reader where

import Control.Effect.Class (LiftIns (LiftIns), type (~>))
import Control.Effect.Class.HFunctor (HFunctor)
import Control.Effect.Class.Reader (AskF (Ask), LocalH (Local), ask)
import Control.Heftia (interpret, reinterpret)
import Control.Heftia.Final (Hef)
import Data.Hefty.Sum (Sum)

interpretReader :: HFunctor (Sum es) => r -> Hef (LocalH r ': LiftIns (AskF r) ': es) ~> Hef es
interpretReader r = interpretAsk r . elaborateReader

elaborateReader ::
    HFunctor (Sum es) =>
    Hef (LocalH r ': LiftIns (AskF r) ': es) ~> Hef (LiftIns (AskF r) ': es)
elaborateReader = interpret \(Local f a) ->
    ($ a) $ reinterpret \(LiftIns Ask) -> f <$> ask

interpretAsk :: HFunctor (Sum es) => r -> Hef (LiftIns (AskF r) ': es) ~> Hef es
interpretAsk r = interpret \(LiftIns Ask) -> pure r
