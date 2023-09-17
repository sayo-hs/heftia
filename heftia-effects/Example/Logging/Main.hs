{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Main where

import Control.Effect.Class (SendIns (sendIns), type (<:), type (~>))
import Control.Effect.Class.Machinery.TH (makeEffectF, makeEffectH)
import Control.Effect.Class.Reader (Ask (ask), AskI, Local (local), LocalS)
import Control.Effect.Class.State (State (get), StateI, modify)
import Control.Effect.Freer (
    Fre,
    flipFreer,
    interpose,
    interpret,
    interpreted,
    liftLower,
    raise,
    raise2,
    runFreerEffects,
    subsume,
    type (<|),
 )
import Control.Effect.Handler.Heftia.Reader (interpretReader, liftReader)
import Control.Effect.Handler.Heftia.State (evalState)
import Control.Effect.Heftia (
    ForallHFunctor,
    Hef,
    elaborated,
    flipHeftia,
    hoistHeftiaEffects,
    hoistInterpose,
    interpretH,
    liftLowerH,
    runElaborate,
    type (<<|),
 )
import Control.Monad (when)
import Control.Monad.Trans.Heftia.Church (HeftiaChurchT)
import Data.Function ((&))
import Data.Hefty.Extensible (ExtensibleUnionH)
import Data.Hefty.Union (absurdUnionH, (|+:))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Prelude hiding (log)

class Log f where
    log :: Text -> f ()

makeEffectF ''Log

logToIO :: (IO <: Fre r m, Monad m) => Fre (LogI ': r) m ~> Fre r m
logToIO = interpret \(Log msg) -> sendIns $ T.putStrLn msg

class Time f where
    currentTime :: f UTCTime

makeEffectF ''Time

timeToIO :: (IO <: Fre r m, Monad m) => Fre (TimeI ': r) m ~> Fre r m
timeToIO = interpret \case
    CurrentTime -> sendIns getCurrentTime

logWithTime :: (LogI <| es, Time (Fre es m), Monad m) => Fre es m ~> Fre es m
logWithTime = interpose \(Log msg) -> do
    t <- currentTime
    log $ "[" <> T.pack (show t) <> "] " <> msg

-- | An effect that introduces a scope that represents a chunk of logs.
class LogChunk f where
    logChunk :: f a -> f a

makeEffectH ''LogChunk

-- | Output logs in log chunks as they are.
passthroughLogChunk ::
    (Monad m, ForallHFunctor r) =>
    Hef (LogChunkS ': r) m ~> Hef r m
passthroughLogChunk = interpretH \(LogChunk m) -> m

-- | Limit the number of logs in a log chunk to the first @n@ logs.
limitLogChunk ::
    forall m.
    (LogChunk m, Log m, Monad m) =>
    Int ->
    LogChunkS (Fre '[LogI] m) ~> m
limitLogChunk n (LogChunk a) =
    logChunk
        . interpreted
        . evalState 0
        . interpretLog
        . flipFreer
        . raise
        $ a
  where
    interpretLog :: Fre '[LogI, StateI Int] m ~> Fre '[StateI Int] m
    interpretLog =
        interpret \(Log msg) -> do
            count <- get
            when (count <= n) do
                liftLower $
                    if count == n
                        then log "LOG OMITTED..."
                        else log msg

                modify @Int (+ 1)

class FileSystem f where
    mkdir :: FilePath -> f ()
    writeFS :: FilePath -> String -> f ()

makeEffectF ''FileSystem

runDummyFS :: (IO <: Fre r m, Monad m) => Fre (FileSystemI ': r) m ~> Fre r m
runDummyFS = interpret \case
    Mkdir path -> sendIns $ putStrLn $ "<runDummyFS> mkdir " <> path
    WriteFS path content -> sendIns $ putStrLn $ "<runDummyFS> writeFS " <> path <> " : " <> content

-- | Create directories according to the log-chunk structure and save one log in one file.
saveLogChunk ::
    forall es es' m.
    ( LogChunkS <<| es
    , LogI <| es'
    , FileSystem (Fre es' m)
    , Time (Fre es' m)
    , Monad m
    , ForallHFunctor es
    ) =>
    Hef (LogChunkS ': es) (Fre (LogI ': es') m) ~> Hef es (Fre (LogI ': es') m)
saveLogChunk =
    interpretReader ("./log_chunks/" :: FilePath)
        . hoistHeftiaEffects flipFreer
        . interpretLogChunk
        . hoistHeftiaEffects flipFreer
        . flipHeftia
        . liftReader
  where
    interpretLogChunk ::
        Hef (LogChunkS ': LocalS FilePath ': es) (Fre (LogI ': AskI FilePath ': es') m)
            ~> Hef (LocalS FilePath ': es) (Fre (LogI ': AskI FilePath ': es') m)
    interpretLogChunk =
        interpretH \(LogChunk a) -> logChunk do
            chunkBeginAt <- currentTime & raise2 & liftLowerH
            local @FilePath (++ iso8601Show chunkBeginAt ++ "/") do
                newLogChunkDir <- ask & liftLowerH
                mkdir newLogChunkDir & raise2 & liftLowerH
                a & hoistInterpose \(Log msg) -> do
                    logAt <- currentTime & raise2
                    saveDir <- ask
                    log msg & raise2
                    writeFS (saveDir ++ iso8601Show logAt ++ ".log") (show msg) & raise2

logs :: (LogChunk m, Log m, IO <: m, Monad m) => m ()
logs =
    logChunk do
        log "foo"
        log "bar"
        log "baz"
        log "qux"

        sendIns $ putStrLn "------"

        logChunk do
            log "hoge"
            log "piyo"
            log "fuga"
            log "hogera"

        sendIns $ putStrLn "------"

        log "quux"
        log "foobar"

main :: IO ()
main =
    runFreerEffects
        . logToIO
        . timeToIO
        . logWithTime
        . runDummyFS
        . subsume
        . elaborated
        . passthroughLogChunk
        . saveLogChunk
        . interpreted
        . subsume
        . runElaborate @_ @HeftiaChurchT @ExtensibleUnionH
            (liftLower . limitLogChunk 2 |+: absurdUnionH)
        $ logs
