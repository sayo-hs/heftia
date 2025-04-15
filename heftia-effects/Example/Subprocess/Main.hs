-- SPDX-License-Identifier: MPL-2.0

module Main where

import Control.Monad.Hefty (liftIO, (&))
import Control.Monad.Hefty.Concurrent.Subprocess (CreateProcess (stdout), StdStream (CreatePipe), SubprocResult, readStdout, runSubprocIO, scoped, shell)
import Control.Monad.Hefty.Unlift (runUnliftIO)

main :: IO ()
main = runUnliftIO . runSubprocIO $ do
    r <- scoped @SubprocResult (shell "echo a b c") {stdout = CreatePipe} \_ -> do
        readStdout
    print r & liftIO

-- SubprocScopeResult ExitSuccess "a b c\n"
