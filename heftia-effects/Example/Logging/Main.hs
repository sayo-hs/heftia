{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Main where

import Control.Arrow ((>>>))
import Control.Effect (type (<:), type (<<:), type (~>))
import Control.Effect.ExtensibleFinal (runEff, type (!!), type (:!!))
import Control.Effect.Hefty (
    Elab,
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
import Control.Effect.Interpreter.Heftia.Reader (runReader)
import Control.Effect.Interpreter.Heftia.State (evalState)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Effect.Reader (LAsk, Local, ask, local)
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
import Data.Time.Format (defaultTimeLocale, formatTime)

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
    logging $ "[" <> iso8601 t <> "] " <> msg

iso8601 :: UTCTime -> Text
iso8601 t = T.take 23 (T.pack $ formatTime defaultTimeLocale "%FT%T.%q" t) <> "Z"

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

data FileSystem a where
    Mkdir :: FilePath -> FileSystem ()
    WriteToFile :: FilePath -> Text -> FileSystem ()
makeEffectF [''FileSystem]

runDummyFS :: (IO <| r, ForallHFunctor eh) => eh :!! LFileSystem ': r ~> eh :!! r
runDummyFS = interpretRec \case
    Mkdir path ->
        liftIO $ putStrLn $ "<runDummyFS> mkdir " <> path
    WriteToFile path content ->
        liftIO $ putStrLn $ "<runDummyFS> writeToFile " <> path <> " : " <> T.unpack content

-- | Create directories according to the log-chunk structure and save one log in one file.
saveLogChunk ::
    forall eh ef.
    (LogChunk <<| eh, Log <| ef, FileSystem <| ef, Time <| ef, ForallHFunctor eh) =>
    eh :!! ef ~> eh :!! ef
saveLogChunk =
    raise >>> raiseH
        >>> hookCreateDirectory
        >>> hookWriteFile
        >>> runReader @FilePath "./log/"
  where
    hookCreateDirectory
        , hookWriteFile ::
            (Local FilePath ': eh :!! LAsk FilePath ': ef)
                ~> (Local FilePath ': eh :!! LAsk FilePath ': ef)
    hookCreateDirectory =
        interposeRecH \(LogChunk chunkName a) -> logChunk chunkName do
            chunkBeginAt <- currentTime
            let dirName = T.unpack $ iso8601 chunkBeginAt <> "-" <> chunkName
            local @FilePath (++ dirName ++ "/") do
                logChunkPath <- ask
                mkdir logChunkPath
                a

    hookWriteFile =
        interposeRec \(Logging msg) -> do
            logChunkPath <- ask
            logAt <- currentTime
            writeToFile (T.unpack $ T.pack logChunkPath <> iso8601 logAt <> ".log") msg
            logging msg

-- | Limit the number of logs in a log chunk to the first @n@ logs.
limitLogChunk :: Log <| ef => Int -> '[LogChunk] :!! LLog ': ef ~> '[LogChunk] :!! LLog ': ef
limitLogChunk n = reinterpretRecH $ elabLimitLogChunk n

elabLimitLogChunk :: Log <| ef => Int -> Elab LogChunk ('[LogChunk] :!! LLog ': ef)
elabLimitLogChunk n (LogChunk name a) =
    logChunk name do
        raise . raiseH $ limitLog $ runLogChunk $ limitLogChunk n a
  where
    limitLog :: Log <| ef => '[] :!! LLog ': ef ~> '[] :!! ef
    limitLog a' =
        evalState @Int 0 $
            raiseUnder a' & interpretRec \(Logging msg) -> do
                count <- get
                when (count < n) do
                    logging msg
                    when (count == n - 1) do
                        logging "Subsequent logs are omitted..."

                    modify @Int (+ 1)

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
        & saveLogChunk
        & limitLogChunk 2
        & subsume @LLog
        & runApp

limitThenSave :: IO ()
limitThenSave =
    logExample
        & limitLogChunk 2
        & subsume @LLog
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
<runDummyFS> writeToFile ./log/2024-08-31T17:25:38.063Z.log : out of chunk scope 1
[2024-08-31T17:25:38.063Z] out of chunk scope 1
<runDummyFS> writeToFile ./log/2024-08-31T17:25:38.063Z.log : out of chunk scope 2
[2024-08-31T17:25:38.063Z] out of chunk scope 2
<runDummyFS> writeToFile ./log/2024-08-31T17:25:38.063Z.log : out of chunk scope 3
[2024-08-31T17:25:38.063Z] out of chunk scope 3
<runDummyFS> writeToFile ./log/2024-08-31T17:25:38.063Z.log : out of chunk scope 4
[2024-08-31T17:25:38.063Z] out of chunk scope 4
------
<runDummyFS> mkdir ./log/2024-08-31T17:25:38.063Z-scope1/
<runDummyFS> writeToFile ./log/2024-08-31T17:25:38.063Z-scope1/2024-08-31T17:25:38.063Z.log : in scope1 1
[2024-08-31T17:25:38.063Z] in scope1 1
<runDummyFS> writeToFile ./log/2024-08-31T17:25:38.063Z-scope1/2024-08-31T17:25:38.063Z.log : in scope1 2
[2024-08-31T17:25:38.063Z] in scope1 2
[2024-08-31T17:25:38.064Z] Subsequent logs are omitted...
<runDummyFS> writeToFile ./log/2024-08-31T17:25:38.063Z-scope1/2024-08-31T17:25:38.064Z.log : in scope1 3
<runDummyFS> writeToFile ./log/2024-08-31T17:25:38.063Z-scope1/2024-08-31T17:25:38.064Z.log : in scope1 4
------
<runDummyFS> mkdir ./log/2024-08-31T17:25:38.063Z-scope1/2024-08-31T17:25:38.064Z-scope2/
<runDummyFS> writeToFile ./log/2024-08-31T17:25:38.063Z-scope1/2024-08-31T17:25:38.064Z-scope2/2024-08-31T17:25:38.064Z.log : in scope2 1
[2024-08-31T17:25:38.064Z] in scope2 1
<runDummyFS> writeToFile ./log/2024-08-31T17:25:38.063Z-scope1/2024-08-31T17:25:38.064Z-scope2/2024-08-31T17:25:38.064Z.log : in scope2 2
[2024-08-31T17:25:38.064Z] in scope2 2
[2024-08-31T17:25:38.064Z] Subsequent logs are omitted...
<runDummyFS> writeToFile ./log/2024-08-31T17:25:38.063Z-scope1/2024-08-31T17:25:38.064Z-scope2/2024-08-31T17:25:38.064Z.log : in scope2 3
<runDummyFS> writeToFile ./log/2024-08-31T17:25:38.063Z-scope1/2024-08-31T17:25:38.064Z-scope2/2024-08-31T17:25:38.064Z.log : in scope2 4
------
<runDummyFS> writeToFile ./log/2024-08-31T17:25:38.063Z-scope1/2024-08-31T17:25:38.064Z.log : in scope1 5
<runDummyFS> writeToFile ./log/2024-08-31T17:25:38.063Z-scope1/2024-08-31T17:25:38.064Z.log : in scope1 6

# limitThenSave
<runDummyFS> writeToFile ./log/2024-08-31T17:25:38.064Z.log : out of chunk scope 1
[2024-08-31T17:25:38.065Z] out of chunk scope 1
<runDummyFS> writeToFile ./log/2024-08-31T17:25:38.065Z.log : out of chunk scope 2
[2024-08-31T17:25:38.065Z] out of chunk scope 2
<runDummyFS> writeToFile ./log/2024-08-31T17:25:38.065Z.log : out of chunk scope 3
[2024-08-31T17:25:38.065Z] out of chunk scope 3
<runDummyFS> writeToFile ./log/2024-08-31T17:25:38.065Z.log : out of chunk scope 4
[2024-08-31T17:25:38.065Z] out of chunk scope 4
------
<runDummyFS> mkdir ./log/2024-08-31T17:25:38.065Z-scope1/
<runDummyFS> writeToFile ./log/2024-08-31T17:25:38.065Z-scope1/2024-08-31T17:25:38.065Z.log : in scope1 1
[2024-08-31T17:25:38.065Z] in scope1 1
<runDummyFS> writeToFile ./log/2024-08-31T17:25:38.065Z-scope1/2024-08-31T17:25:38.065Z.log : in scope1 2
[2024-08-31T17:25:38.065Z] in scope1 2
<runDummyFS> writeToFile ./log/2024-08-31T17:25:38.065Z-scope1/2024-08-31T17:25:38.065Z.log : Subsequent logs are omitted...
[2024-08-31T17:25:38.065Z] Subsequent logs are omitted...
------
<runDummyFS> writeToFile ./log/2024-08-31T17:25:38.065Z-scope1/2024-08-31T17:25:38.065Z.log : in scope2 1
[2024-08-31T17:25:38.065Z] in scope2 1
<runDummyFS> writeToFile ./log/2024-08-31T17:25:38.065Z-scope1/2024-08-31T17:25:38.065Z.log : in scope2 2
[2024-08-31T17:25:38.065Z] in scope2 2
<runDummyFS> writeToFile ./log/2024-08-31T17:25:38.065Z-scope1/2024-08-31T17:25:38.065Z.log : Subsequent logs are omitted...
[2024-08-31T17:25:38.065Z] Subsequent logs are omitted...
------
-}
