{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- SPDX-License-Identifier: MPL-2.0

module Main where

import Control.Arrow ((>>>))
import Control.Effect.Transform (subsumeUnder)
import Control.Monad (when)
import Control.Monad.Hefty (
    Eff,
    Effect,
    Emb,
    FOEs,
    interpose,
    interpret,
    liftIO,
    makeEffectF,
    makeEffectH,
    raise,
    raiseUnder,
    reinterpret,
    runEff,
    (&),
    type (:>),
    type (~>),
    type (~~>),
 )
import Control.Monad.Hefty.Reader (runReader)
import Control.Monad.Hefty.State (evalState)
import Data.Effect.Reader (Ask, Local, ask, local)
import Data.Effect.State (get, modify)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)

data Log :: Effect where
    Logging :: Text -> Log f ()
makeEffectF ''Log

logToIO :: (Emb IO :> es) => Eff (Log ': es) ~> Eff es
logToIO = interpret \(Logging msg) -> liftIO $ T.putStrLn msg

data Time :: Effect where
    CurrentTime :: Time f UTCTime
makeEffectF ''Time

timeToIO :: (Emb IO :> es) => Eff (Time ': es) ~> Eff es
timeToIO = interpret \CurrentTime -> liftIO getCurrentTime

logWithTime :: (Log :> es, Time :> es) => Eff es ~> Eff es
logWithTime = interpose \(Logging msg) -> do
    t <- currentTime
    logging $ "[" <> iso8601 t <> "] " <> msg

iso8601 :: UTCTime -> Text
iso8601 t = T.take 23 (T.pack $ formatTime defaultTimeLocale "%FT%T.%q" t) <> "Z"

-- | An effect that introduces a scope that represents a chunk of logs.
data LogChunk :: Effect where
    LogChunk
        :: Text
        -- ^ chunk name
        -> f a
        -> LogChunk f a

makeEffectH ''LogChunk

-- | Ignore chunk names and output logs in log chunks as they are.
runLogChunk :: Eff (LogChunk ': es) ~> Eff es
runLogChunk = interpret \(LogChunk _ m) -> m

data FileSystem :: Effect where
    Mkdir :: FilePath -> FileSystem f ()
    WriteToFile :: FilePath -> Text -> FileSystem f ()
makeEffectF ''FileSystem

runDummyFS :: (Emb IO :> es) => Eff (FileSystem ': es) ~> Eff es
runDummyFS = interpret \case
    Mkdir path ->
        liftIO $ putStrLn $ "<runDummyFS> mkdir " <> path
    WriteToFile path content ->
        liftIO $ putStrLn $ "<runDummyFS> writeToFile " <> path <> " : " <> T.unpack content

-- | Create directories according to the log-chunk structure and save one log in one file.
saveLogChunk
    :: forall es
     . (LogChunk :> es, Log :> es, FileSystem :> es, Time :> es)
    => Eff es ~> Eff es
saveLogChunk =
    raise
        >>> raise
        >>> hookCreateDirectory
        >>> hookWriteFile
        >>> runReader @FilePath "./log/"
  where
    hookCreateDirectory
        , hookWriteFile
            :: Eff (Local FilePath ': Ask FilePath ': es)
                ~> Eff (Local FilePath ': Ask FilePath ': es)
    hookCreateDirectory =
        interpose \(LogChunk chunkName a) -> logChunk chunkName do
            chunkBeginAt <- currentTime
            let dirName = T.unpack $ iso8601 chunkBeginAt <> "-" <> chunkName
            local @FilePath (++ dirName ++ "/") do
                logChunkPath <- ask
                mkdir logChunkPath
                a

    hookWriteFile =
        interpose \(Logging msg) -> do
            logChunkPath <- ask
            logAt <- currentTime
            writeToFile (T.unpack $ T.pack logChunkPath <> iso8601 logAt <> ".log") msg
            logging msg

-- | Limit the number of logs in a log chunk to the first @n@ logs.
limitLogChunk :: forall es. (Log :> es, FOEs es) => Int -> Eff (LogChunk ': Log ': es) ~> Eff (LogChunk ': Log ': es)
limitLogChunk n = reinterpret $ handleLimitLogChunk n

handleLimitLogChunk :: (Log :> es, FOEs es) => Int -> LogChunk ~~> Eff (LogChunk ': Log ': es)
handleLimitLogChunk n (LogChunk name a) =
    logChunk name do
        raise . raise $ limitLog $ runLogChunk $ limitLogChunk n a
  where
    limitLog :: (Log :> es, FOEs es) => Eff (Log ': es) ~> Eff es
    limitLog a' =
        evalState @Int 0 $
            raiseUnder a' & interpret \(Logging msg) -> do
                count <- get
                when (count < n) do
                    logging msg
                    when (count == n - 1) do
                        logging "Subsequent logs are omitted..."

                    modify @Int (+ 1)

logExample :: (LogChunk :> es, Log :> es, Emb IO :> es) => Eff es ()
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
        & subsumeUnder
        & runApp

limitThenSave :: IO ()
limitThenSave =
    logExample
        & limitLogChunk 2
        & subsumeUnder
        & saveLogChunk
        & runApp

runApp :: Eff '[LogChunk, FileSystem, Time, Log, Emb IO] ~> IO
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
