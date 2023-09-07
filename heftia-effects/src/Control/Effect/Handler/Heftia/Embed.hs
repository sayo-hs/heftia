-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Effect.Handler.Heftia.Embed where

import Control.Effect.Class (type (~>))
import Control.Effect.Class.Embed (EmbedI (Embed))
import Control.Effect.Freer (Fre, FreerEffects, interpret, interpreted, liftLower)
import Control.Freer.Trans (TransFreer)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Free.Union (Union)

runEmbed :: (TransFreer c fr, c f, Union u) => FreerEffects fr u '[EmbedI f] f ~> f
runEmbed = interpreted . interpret \(Embed a) -> liftLower a

runEmbedIO :: MonadIO m => Fre (EmbedI IO ': es) m ~> Fre es m
runEmbedIO = interpret \(Embed m) -> liftLower $ liftIO m
