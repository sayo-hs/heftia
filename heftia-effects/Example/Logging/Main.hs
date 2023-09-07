{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Main where

import Control.Effect.Class (type (~>))
import Control.Effect.Class.Embed (Embed, embed)
import Control.Effect.Class.Machinery.HFunctor (HFunctor)
import Control.Effect.Class.Machinery.TH (makeEffectF, makeEffectH)
import Control.Effect.Class.Reader (Ask, ask)
import Control.Effect.Class.State (State (..), modify)
import Control.Effect.Freer (Fre, interpose, interpret, raise, type (<:))
import Control.Effect.Handler.Heftia.Embed (runEmbed)
import Control.Effect.Handler.Heftia.Reader (interpretAsk)
import Control.Effect.Handler.Heftia.State (evalState)
import Control.Effect.Heftia (Hef, interpretH, runElaborate)
import Control.Monad (when)
import Control.Monad.Extra (whenM)
import Control.Monad.Trans.Heftia.Church (HeftiaChurchT)
import Data.Functor ((<&>))
import Data.Hefty.Sum (SumH, SumUnionH)
import Data.Hefty.Union
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Time (UTCTime, getCurrentTime)
import System.LogLevel (LogLevel (..))
import Prelude hiding (log)

class Log f where
    log :: LogLevel -> Text -> f ()
makeEffectF ''Log

logToIO ::
    (Embed IO (Fre r m), Ask LogLevel (Fre r m), Monad m) =>
    Fre (LogI ': r) m ~> Fre r m
logToIO = interpret \case
    Log level msg -> do
        currentLevel <- ask
        when (level <= currentLevel) do
            embed $ T.putStrLn msg

class Time f where
    currentTime :: f UTCTime
makeEffectF ''Time

timeToIO :: (Embed IO (Fre r m), Monad m) => Fre (TimeI ': r) m ~> Fre r m
timeToIO = interpret \case
    CurrentTime -> embed getCurrentTime

logWithMetadata :: (LogI <: es, Time (Fre es m), Monad m) => Fre es m ~> Fre es m
logWithMetadata = interpose \(Log level msg) -> do
    t <- currentTime
    log level $ "[" <> T.pack (show level) <> " " <> T.pack (show t) <> "] " <> msg

class LogChunk f where
    logChunk :: f a -> f a
makeEffectH ''LogChunk

passthroughLogChunk ::
    HFunctor (SumH r) =>
    Hef (LogChunkS ': r) (Fre r' m) ~> Hef r (Fre r' m)
passthroughLogChunk = interpretH \(LogChunk m) -> m

limitLogChunk :: (LogI <: es, Monad m) => Int -> LogChunkS (Fre es m) ~> Fre es m
limitLogChunk n (LogChunk m) =
    evalState @Int 0
        . ($ raise m)
        $ interpose \(Log level msg) ->
            whenM (get <&> (< n)) do
                log level msg
                modify @Int (+ 1)

main :: IO ()
main = runEmbed
    . interpretAsk Debug
    . logToIO
    . timeToIO
    . logWithMetadata
    . runElaborate @Monad @HeftiaChurchT
        (limitLogChunk 2 |+: absurdUnionH @SumUnionH)
    $ do
        logChunk do
            log Warn "WARNING!!"
            log Error "ERROR!!"
            log Info "HELLO"
            log Debug "Would you like some coffee?"
