-- SPDX-License-Identifier: MPL-2.0

module Test.Writer where

import Control.Effect (type (<:), type (<<:))
import Control.Monad.Hefty.Interpret (runEff)
import Control.Monad.Hefty.Writer (runTell, runWriterHPost, runWriterHPre)
import Data.Effect.Writer (Tell, WriterH, censor, tell)
import Test.Hspec (Spec, describe, it, shouldBe)

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

spec_Writer_Elaboration :: Spec
spec_Writer_Elaboration = describe "Elaboration for Writer" do
    it "Pre-applying Cencor" do
        (sPre, ()) <- runEff . runTell . runWriterHPre @String $ censorHello
        sPre `shouldBe` "Goodbye world!"
    it "Post-applying Cencor" do
        (sPre, ()) <- runEff . runTell . runWriterHPost @String $ censorHello
        sPre `shouldBe` "Hello world!!"
