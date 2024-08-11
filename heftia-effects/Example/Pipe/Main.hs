-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Main where

import Control.Effect.Handler.Heftia.Concurrent.Pipe.Async (runAsyncPipe)
import Control.Effect.Handler.Heftia.Concurrent.Pipe.MVar (runMVarPipeLine)
import Control.Effect.Handler.Heftia.Unlift (runUnliftIO)
import Control.Monad.IO.Class (liftIO)
import Data.Effect.Concurrent.Pipe (
    consume,
    feed,
    passthrough,
    pipeLoop,
    unmaskPipe,
    (*|>),
    (|*>),
    (|>),
 )

main :: IO ()
main = runUnliftIO
    . runAsyncPipe
    . runMVarPipeLine @String
    $ do
        unmaskPipe @String do
            _ <- feed "direct pipe test" |> (liftIO . putStrLn =<< consume)
            _ <- feed "passthrough test" *|> passthrough |*> (liftIO . putStrLn =<< consume)
            _ <- pipeLoop @String do
                feed "loop test"
                liftIO . putStrLn =<< consume
            pure ()

{- result:
direct pipe test
passthrough test
loop test
-}
