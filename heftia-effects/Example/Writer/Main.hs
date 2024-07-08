-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Main where

import Control.Effect (type (<:), type (<<:))
import Control.Effect.ExtensibleChurch (runEff)
import Control.Effect.Handler.Heftia.Writer (elaborateWriterPost, elaborateWriterPre, interpretTell)
import Control.Effect.Hefty (interpretH)
import Control.Monad.IO.Class (liftIO)
import Data.Effect.Writer (Tell, WriterH, censor, tell)

hello :: (Tell String <: m, Monad m) => m ()
hello = do
    tell "Hello"
    tell " world!"

censorHello :: (Tell String <: m, WriterH String <<: m, Monad m) => m ()
censorHello =
    censor
        ( \s ->
            if s == "Hello"
                then "Goodbye"
                else
                    if s == "Hello world!"
                        then "Hello world!!"
                        else s
        )
        hello

main :: IO ()
main = runEff do
    (sPre, _) <-
        interpretTell
            . interpretH (elaborateWriterPre @String)
            $ censorHello

    (sPost, _) <-
        interpretTell
            . interpretH (elaborateWriterPost @String)
            $ censorHello

    liftIO $ putStrLn $ "Pre-applying: " <> sPre
    liftIO $ putStrLn $ "Post-applying: " <> sPost

{-
Pre-applying: Goodbye world!
Post-applying: Hello world!!
-}
