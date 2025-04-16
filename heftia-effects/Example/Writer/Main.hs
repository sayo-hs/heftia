-- SPDX-License-Identifier: MPL-2.0

module Main where

import Control.Monad.Hefty (Eff, liftIO, runEff, type (:>))
import Control.Monad.Hefty.Writer (runTell, runWriterHPost, runWriterHPre)
import Data.Effect.Writer (Tell, WriterH, censor, tell)

hello :: (Tell String :> es) => Eff es ()
hello = do
    tell "Hello"
    tell " world!"

censorHello :: (Tell String :> es, WriterH String :> es) => Eff es ()
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
        runTell
            . runWriterHPre @String
            $ censorHello

    (sPost, _) <-
        runTell
            . runWriterHPost @String
            $ censorHello

    liftIO $ putStrLn $ "Pre-applying: " <> sPre
    liftIO $ putStrLn $ "Post-applying: " <> sPost

{-
Pre-applying: Goodbye world!
Post-applying: Hello world!!
-}
