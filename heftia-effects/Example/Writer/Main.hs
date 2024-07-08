-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Main where

import Control.Effect (sendIns, type (<:), type (<<:))
import Control.Effect.ExtensibleChurch (runEff)
import Control.Effect.Handler.Heftia.Writer (elaborateWriterPost, elaborateWriterPre, interpretTell)
import Control.Effect.Hefty (
    interpretH,
 )
import Data.Effect.Writer (Tell, WriterH, censor, tell)

hello :: (Tell String <: m, Monad m) => m ()
hello = do
    tell "Hello"
    tell " world!"

censorHello :: (Tell String <: m, WriterH String <<: m, Monad m) => m ()
censorHello =
    censor
        ( \s ->
            if s == "Hello" then
                "Goodbye"
            else if s == "Hello world!" then
                "Hello world!!"
            else
                s
        )
        hello

main :: IO ()
main = runEff do
    (sPre :: String, _) <-
        interpretTell
            . interpretH (elaborateWriterPre @String)
            $ censorHello

    (sPost :: String, _) <-
        interpretTell
            . interpretH (elaborateWriterPost @String)
            $ censorHello

    sendIns $ putStrLn $ "Pre-applying: " <> sPre
    sendIns $ putStrLn $ "Post-applying: " <> sPost

{-
Pre-applying: Goodbye world!
Post-applying: Hello world!!
-}
