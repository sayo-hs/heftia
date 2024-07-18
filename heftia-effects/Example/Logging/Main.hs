{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Main where

import Control.Arrow ((>>>))
import Control.Effect (type (<:), type (<<:), type (~>))
import Control.Effect.ExtensibleChurch (runEff, type (!!), type (:!!))
import Control.Effect.Handler.Heftia.Reader (runReader)
import Control.Effect.Handler.Heftia.State (evalState)
import Control.Effect.Hefty (
    Elab,
    copyEff,
    interposeRec,
    interposeRecH,
    interpretRec,
    interpretRecH,
    raise,
    raiseH,
    raiseUnder,
    reinterpretRecH,
    subsume,
 )
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Effect.Reader (ask, local)
import Data.Effect.State (get, modify)
import Data.Effect.TH (makeEffectF, makeEffectH)
import Data.Free.Sum (type (+))
import Data.Function ((&))
import Data.Hefty.Extensible (ForallHFunctor, type (<<|), type (<|))
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)

data Log a where
    Logging :: Text -> Log ()

makeEffectF [''Log]

logToIO :: (IO <| r, ForallHFunctor eh) => eh :!! LLog ': r ~> eh :!! r
logToIO = interpretRec \(Logging msg) -> liftIO $ T.putStrLn msg

data Time a where
    CurrentTime :: Time UTCTime

makeEffectF [''Time]

timeToIO :: (IO <| r, ForallHFunctor eh) => eh :!! LTime ': r ~> eh :!! r
timeToIO = interpretRec \CurrentTime -> liftIO getCurrentTime

logWithTime :: (Log <| ef, Time <| ef, ForallHFunctor eh) => eh :!! ef ~> eh :!! ef
logWithTime = interposeRec \(Logging msg) -> do
    t <- currentTime
    logging $ "[" <> T.pack (show t) <> "] " <> msg

-- | An effect that introduces a scope that represents a chunk of logs.
data LogChunk f (a :: Type) where
    LogChunk ::
        -- | chunk name
        Text ->
        f a ->
        LogChunk f a

makeEffectH [''LogChunk]

-- | Ignore chunk names and output logs in log chunks as they are.
runLogChunk :: ForallHFunctor eh => LogChunk ': eh :!! ef ~> eh :!! ef
runLogChunk = interpretRecH \(LogChunk _ m) -> m

-- | Limit the number of logs in a log chunk to the first @n@ logs.
limitLogChunk :: Log <| ef => Int -> '[LogChunk] :!! LLog ': ef ~> '[LogChunk] :!! LLog ': ef
limitLogChunk n = reinterpretRecH $ elabLimitLogChunk n

elabLimitLogChunk :: Log <| ef => Int -> Elab LogChunk ('[LogChunk] :!! LLog ': ef)
elabLimitLogChunk n (LogChunk name a) =
    logChunk name do
        raise . raiseH $ limitLog $ runLogChunk $ limitLogChunk n a
  where
    limitLog ::
        forall ef.
        Log <| ef =>
        '[] :!! LLog ': ef ~> '[] :!! ef
    limitLog a' =
        evalState @Int 0 $
            raiseUnder a' & interpretRec \(Logging msg) -> do
                count <- get
                when (count < n) do
                    logging msg
                    when (count == n - 1) do
                        logging "Subsequent logs are omitted..."

                    modify @Int (+ 1)

data FileSystem a where
    Mkdir :: FilePath -> FileSystem ()
    WriteToFile :: FilePath -> String -> FileSystem ()

makeEffectF [''FileSystem]

runDummyFS :: (IO <| r, ForallHFunctor eh) => eh :!! LFileSystem ': r ~> eh :!! r
runDummyFS = interpretRec \case
    Mkdir path ->
        liftIO $ putStrLn $ "<runDummyFS> mkdir " <> path
    WriteToFile path content ->
        liftIO $ putStrLn $ "<runDummyFS> writeToFile " <> path <> " : " <> content

-- | Create directories according to the log-chunk structure and save one log in one file.
saveLogChunk ::
    forall eh ef.
    (LogChunk <<| eh, FileSystem <| ef, Time <| ef, ForallHFunctor eh) =>
    eh :!! (LLog ': ef) ~> eh :!! ef
saveLogChunk =
    raise >>> raiseH
        >>> ( interposeRecH \(LogChunk chunkName a) -> logChunk chunkName do
                chunkBeginAt <- currentTime
                let dirName = iso8601Show chunkBeginAt ++ "-" ++ T.unpack chunkName
                local @FilePath (++ dirName ++ "/") do
                    logChunkPath <- ask
                    mkdir logChunkPath
                    a & interposeRec \(Logging msg) -> do
                        logAt <- currentTime
                        writeToFile (logChunkPath ++ iso8601Show logAt ++ ".log") (show msg)
            )
        >>> runReader @FilePath "./log/"
        >>> discardLog

discardLog :: ForallHFunctor eh => eh :!! LLog ': r ~> eh :!! r
discardLog = interpretRec \(Logging _) -> pure ()

logExample :: (LogChunk <<: m, Log <: m, MonadIO m) => m ()
logExample = do
    logging "out of chunk scope 1"
    logging "out of chunk scope 2"
    logging "out of chunk scope 3"
    logging "out of chunk scope 4"

    liftIO $ putStrLn "------"

    logChunk "scope1" do
        logging "in scope1 1"
        logging "in scope1 2"
        logging "in scope1 3"
        logging "in scope1 4"

        liftIO $ putStrLn "------"

        logChunk "scope2" do
            logging "in scope2 1"
            logging "in scope2 2"
            logging "in scope2 3"
            logging "in scope2 4"

        liftIO $ putStrLn "------"

        logging "in scope1 5"
        logging "in scope1 6"

saveThenLimit :: IO ()
saveThenLimit =
    logExample
        & copyEff
        & saveLogChunk
        & limitLogChunk 2
        & subsume
        & runApp

limitThenSave :: IO ()
limitThenSave =
    logExample
        & limitLogChunk 2
        & subsume
        & copyEff
        & saveLogChunk
        & runApp

runApp :: LogChunk !! FileSystem + Time + Log + IO ~> IO
runApp =
    runLogChunk
        >>> runDummyFS
        >>> logWithTime
        >>> timeToIO
        >>> logToIO
        >>> runEff

main :: IO ()
main = do
    putStrLn "# saveThenLimit"
    saveThenLimit
    putStrLn ""
    putStrLn "# limitThenSave"
    limitThenSave

{-
# saveThenLimit
[2024-07-18 08:08:51.172881738 UTC] out of chunk scope 1
[2024-07-18 08:08:51.172960947 UTC] out of chunk scope 2
[2024-07-18 08:08:51.172977057 UTC] out of chunk scope 3
[2024-07-18 08:08:51.172991414 UTC] out of chunk scope 4
------
<runDummyFS> mkdir ./log/2024-07-18T08:08:51.17303165Z-scope1/
<runDummyFS> writeToFile ./log/2024-07-18T08:08:51.17303165Z-scope1/2024-07-18T08:08:51.173071515Z.log : "in scope1 1"
[2024-07-18 08:08:51.173099437 UTC] in scope1 1
<runDummyFS> writeToFile ./log/2024-07-18T08:08:51.17303165Z-scope1/2024-07-18T08:08:51.173117962Z.log : "in scope1 2"
[2024-07-18 08:08:51.173143159 UTC] in scope1 2
[2024-07-18 08:08:51.173157136 UTC] Subsequent logs are omitted...
<runDummyFS> writeToFile ./log/2024-07-18T08:08:51.17303165Z-scope1/2024-07-18T08:08:51.173174057Z.log : "in scope1 3"
<runDummyFS> writeToFile ./log/2024-07-18T08:08:51.17303165Z-scope1/2024-07-18T08:08:51.173208753Z.log : "in scope1 4"
------
<runDummyFS> mkdir ./log/2024-07-18T08:08:51.17303165Z-scope1/2024-07-18T08:08:51.173252996Z-scope2/
<runDummyFS> writeToFile ./log/2024-07-18T08:08:51.17303165Z-scope1/2024-07-18T08:08:51.173252996Z-scope2/2024-07-18T08:08:51.17328742Z.log : "in scope2 1"
[2024-07-18 08:08:51.173320152 UTC] in scope2 1
<runDummyFS> writeToFile ./log/2024-07-18T08:08:51.17303165Z-scope1/2024-07-18T08:08:51.173252996Z-scope2/2024-07-18T08:08:51.173340811Z.log : "in scope2 2"
[2024-07-18 08:08:51.173380014 UTC] in scope2 2
[2024-07-18 08:08:51.173395143 UTC] Subsequent logs are omitted...
<runDummyFS> writeToFile ./log/2024-07-18T08:08:51.17303165Z-scope1/2024-07-18T08:08:51.173252996Z-scope2/2024-07-18T08:08:51.173416954Z.log : "in scope2 3"
<runDummyFS> writeToFile ./log/2024-07-18T08:08:51.17303165Z-scope1/2024-07-18T08:08:51.173252996Z-scope2/2024-07-18T08:08:51.173453883Z.log : "in scope2 4"
------
<runDummyFS> writeToFile ./log/2024-07-18T08:08:51.17303165Z-scope1/2024-07-18T08:08:51.173494499Z.log : "in scope1 5"
<runDummyFS> writeToFile ./log/2024-07-18T08:08:51.17303165Z-scope1/2024-07-18T08:08:51.173522993Z.log : "in scope1 6"

# limitThenSave
[2024-07-18 08:08:51.173568508 UTC] out of chunk scope 1
[2024-07-18 08:08:51.173584158 UTC] out of chunk scope 2
[2024-07-18 08:08:51.173597743 UTC] out of chunk scope 3
[2024-07-18 08:08:51.173611569 UTC] out of chunk scope 4
------
<runDummyFS> mkdir ./log/2024-07-18T08:08:51.173632088Z-scope1/
<runDummyFS> writeToFile ./log/2024-07-18T08:08:51.173632088Z-scope1/2024-07-18T08:08:51.173661753Z.log : "in scope1 1"
[2024-07-18 08:08:51.173684326 UTC] in scope1 1
<runDummyFS> writeToFile ./log/2024-07-18T08:08:51.173632088Z-scope1/2024-07-18T08:08:51.17370233Z.log : "in scope1 2"
[2024-07-18 08:08:51.173728749 UTC] in scope1 2
<runDummyFS> writeToFile ./log/2024-07-18T08:08:51.173632088Z-scope1/2024-07-18T08:08:51.173743437Z.log : "Subsequent logs are omitted..."
[2024-07-18 08:08:51.173763865 UTC] Subsequent logs are omitted...
------
<runDummyFS> writeToFile ./log/2024-07-18T08:08:51.173632088Z-scope1/2024-07-18T08:08:51.173799091Z.log : "in scope2 1"
[2024-07-18 08:08:51.173821103 UTC] in scope2 1
<runDummyFS> writeToFile ./log/2024-07-18T08:08:51.173632088Z-scope1/2024-07-18T08:08:51.173841321Z.log : "in scope2 2"
[2024-07-18 08:08:51.173861538 UTC] in scope2 2
<runDummyFS> writeToFile ./log/2024-07-18T08:08:51.173632088Z-scope1/2024-07-18T08:08:51.173878871Z.log : "Subsequent logs are omitted..."
[2024-07-18 08:08:51.173905952 UTC] Subsequent logs are omitted...
------
-}
