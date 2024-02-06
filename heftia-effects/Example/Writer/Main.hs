-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Main where

import Control.Effect (sendIns, type (<:), type (<<:))
import Control.Effect.ExtensibleFinal (runEff)
import Control.Effect.Handler.Heftia.Writer (elaborateWriter, elaborateWriterTransactional, interpretTell)
import Control.Effect.Hefty (interpretH)
import Data.Effect.Writer (Tell, WriterH, censor, tell)

hello :: (Tell String <: m, Monad m) => m ()
hello = do
    tell "Hello"
    tell " world!"

censorHello :: (Tell String <: m, WriterH String <<: m, Monad m) => m ()
censorHello =
    censor
        (\s -> if s == "Hello" then "Goodbye" else s)
        hello

main :: IO ()
main = runEff do
    (s :: String, _) <-
        interpretTell
            . interpretH (elaborateWriter @String)
            $ censorHello

    (sTransactional :: String, _) <-
        interpretTell
            . interpretH (elaborateWriterTransactional @String)
            $ censorHello

    sendIns $ putStrLn $ "Normal: " <> s
    sendIns $ putStrLn $ "Transactional: " <> sTransactional
