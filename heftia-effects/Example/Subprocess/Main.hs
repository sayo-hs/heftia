-- SPDX-License-Identifier: MPL-2.0
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import Control.Monad.Hefty (liftIO, (&))
import Control.Monad.Hefty.Concurrent.Subprocess (
    CreateProcess (stdout),
    StdStream (CreatePipe),
    SubprocResult,
    readStdout'',
    runSubprocIO,
    scope,
    shell,
 )
import Control.Monad.Hefty.Unlift (runUnliftIO)

main :: IO ()
main = runUnliftIO . runSubprocIO $ do
    r :: SubprocResult p a <-
        scope @"echo" (shell "echo a b c") {stdout = CreatePipe} \_ -> do
            readStdout'' @"echo"
    print r & liftIO
