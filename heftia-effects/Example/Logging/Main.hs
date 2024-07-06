{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Main where
import Data.Text (Text)
import Data.Kind (Type)
import Data.Effect.TH (makeEffectF, makeEffectH)
import Data.Hefty.Extensible (type (<|), type (<<|), ForallHFunctor)
import Control.Effect.ExtensibleChurch (runEff, type (:!!))
import Control.Effect (type (~>), sendIns, type (<:), type (<<:))
import Control.Effect.Hefty (interpretRec, interposeRec, interpretRecH, interposeRec, raise, raiseH, interposeRecH)
import qualified Data.Text.IO as T
import Data.Time (UTCTime, getCurrentTime)
import qualified Data.Text as T
import Data.Effect.State (get, modify)
import Control.Effect.Handler.Heftia.State (evalState)
import Data.Function ((&))
import Control.Monad (when)
import Control.Effect.Handler.Heftia.Reader (interpretReader)
import Data.Effect.Reader (local, ask)
import Data.Time.Format.ISO8601 (iso8601Show)
import Control.Arrow ((>>>))

data Log a where
    Logging :: Text -> Log ()

makeEffectF [''Log]

logToIO :: (IO <| r, ForallHFunctor eh) => eh :!! LLog ': r ~> eh :!! r
logToIO = interpretRec \(Logging msg) -> sendIns $ T.putStrLn msg

data Time a where
    CurrentTime :: Time UTCTime

makeEffectF [''Time]

timeToIO :: (IO <| r, ForallHFunctor eh) => eh :!! LTime ': r ~> eh :!! r
timeToIO = interpretRec \CurrentTime -> sendIns getCurrentTime

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
limitLogChunk
    ::  forall eh ef. (LogChunk <<| eh, Log <| ef) =>
        Int -> LogChunk ('[] :!! ef) ~> LogChunk ('[] :!! ef)
limitLogChunk n (LogChunk chunkName a) =
    LogChunk chunkName . evalState @Int 0 $
        raise a & interposeRec \(Logging msg) -> do
            count <- get
            when (count <= n) do
                if count == n
                    then logging "LOG OMITTED..."
                    else logging msg

                modify @Int (+ 1)

data FileSystem a where
    Mkdir :: FilePath -> FileSystem ()
    WriteToFile :: FilePath -> String -> FileSystem ()

makeEffectF [''FileSystem]

runDummyFS :: (IO <| r, ForallHFunctor eh) => eh :!! LFileSystem ': r ~> eh :!! r
runDummyFS = interpretRec \case
    Mkdir path ->
        sendIns $ putStrLn $ "<runDummyFS> mkdir " <> path
    WriteToFile path content ->
        sendIns $ putStrLn $ "<runDummyFS> writeToFile " <> path <> " : " <> content

-- | Create directories according to the log-chunk structure and save one log in one file.
saveLogChunk
    :: forall eh ef. (LogChunk <<| eh, Log <| ef, FileSystem <| ef, Time <| ef, ForallHFunctor eh) =>
        eh :!! ef ~> eh :!! ef
saveLogChunk =
        raise >>> raiseH
    >>> (   interposeRecH \(LogChunk chunkName a) -> do
                chunkBeginAt <- currentTime
                let dirName = iso8601Show chunkBeginAt ++ "-" ++ T.unpack chunkName
                local @FilePath (++ dirName ++ "/") do
                    logChunkPath <- ask
                    mkdir logChunkPath
                    a & interposeRec \(Logging msg) -> do
                        logAt <- currentTime
                        logging msg
                        writeToFile (logChunkPath ++ iso8601Show logAt ++ ".log") (show msg)
        )
    >>> interpretReader @FilePath "./log/"

logExample :: (LogChunk <<: m, Log <: m, IO <: m, Monad m) => m ()
logExample =
    logChunk "scope1" do
        logging "foo"
        logging "bar"
        logging "baz"
        logging "qux"

        sendIns $ putStrLn "------"

        logChunk "scope2" do
            logging "hoge"
            logging "piyo"
            logging "fuga"
            logging "hogera"

        sendIns $ putStrLn "------"

        logging "quux"
        logging "foobar"

main :: IO ()
main =
    runEff @IO
      . logToIO
      . timeToIO
      . logWithTime
      . runDummyFS
      . runLogChunk
      . saveLogChunk
      $ do
        logExample

{-
<runDummyFS> mkdir ./log/2024-07-06T13:56:23.447829919Z-scope1/
[2024-07-06 13:56:23.448628515 UTC] foo
<runDummyFS> writeToFile ./log/2024-07-06T13:56:23.447829919Z-scope1/2024-07-06T13:56:23.448625419Z.log : "foo"
[2024-07-06 13:56:23.448932798 UTC] bar
<runDummyFS> writeToFile ./log/2024-07-06T13:56:23.447829919Z-scope1/2024-07-06T13:56:23.448930113Z.log : "bar"
[2024-07-06 13:56:23.448989065 UTC] baz
<runDummyFS> writeToFile ./log/2024-07-06T13:56:23.447829919Z-scope1/2024-07-06T13:56:23.448986289Z.log : "baz"
[2024-07-06 13:56:23.449036674 UTC] qux
<runDummyFS> writeToFile ./log/2024-07-06T13:56:23.447829919Z-scope1/2024-07-06T13:56:23.449035743Z.log : "qux"
------
<runDummyFS> mkdir ./log/2024-07-06T13:56:23.447829919Z-scope1/2024-07-06T13:56:23.449090566Z-scope2/
[2024-07-06 13:56:23.44913009 UTC] hoge
<runDummyFS> writeToFile ./log/2024-07-06T13:56:23.447829919Z-scope1/2024-07-06T13:56:23.449127986Z.log : "hoge"
<runDummyFS> writeToFile ./log/2024-07-06T13:56:23.447829919Z-scope1/2024-07-06T13:56:23.449090566Z-scope2/2024-07-06T13:56:23.449125371Z.log : "hoge"
[2024-07-06 13:56:23.449215892 UTC] piyo
<runDummyFS> writeToFile ./log/2024-07-06T13:56:23.447829919Z-scope1/2024-07-06T13:56:23.449213508Z.log : "piyo"
<runDummyFS> writeToFile ./log/2024-07-06T13:56:23.447829919Z-scope1/2024-07-06T13:56:23.449090566Z-scope2/2024-07-06T13:56:23.449210612Z.log : "piyo"
[2024-07-06 13:56:23.449303087 UTC] fuga
<runDummyFS> writeToFile ./log/2024-07-06T13:56:23.447829919Z-scope1/2024-07-06T13:56:23.449300221Z.log : "fuga"
<runDummyFS> writeToFile ./log/2024-07-06T13:56:23.447829919Z-scope1/2024-07-06T13:56:23.449090566Z-scope2/2024-07-06T13:56:23.449298909Z.log : "fuga"
[2024-07-06 13:56:23.449383799 UTC] hogera
<runDummyFS> writeToFile ./log/2024-07-06T13:56:23.447829919Z-scope1/2024-07-06T13:56:23.449380502Z.log : "hogera"
<runDummyFS> writeToFile ./log/2024-07-06T13:56:23.447829919Z-scope1/2024-07-06T13:56:23.449090566Z-scope2/2024-07-06T13:56:23.44937926Z.log : "hogera"
------
[2024-07-06 13:56:23.449513012 UTC] quux
<runDummyFS> writeToFile ./log/2024-07-06T13:56:23.447829919Z-scope1/2024-07-06T13:56:23.449510688Z.log : "quux"
[2024-07-06 13:56:23.449560241 UTC] foobar
<runDummyFS> writeToFile ./log/2024-07-06T13:56:23.447829919Z-scope1/2024-07-06T13:56:23.449558087Z.log : "foobar"
-}
