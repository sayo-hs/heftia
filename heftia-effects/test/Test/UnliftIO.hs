-- SPDX-License-Identifier: MPL-2.0

module Test.UnliftIO where

import Control.Monad.Hefty (liftIO)
import Control.Monad.Hefty.State (evalStateIORef, get)
import Control.Monad.Hefty.Unlift (runUnliftIO, withRunInIO)
import Test.Hspec (Spec, describe, it, shouldBe)

--  https://github.com/tomjaguarpaw/bluefin/issues/29

spec_UnliftIO :: Spec
spec_UnliftIO = describe "MonadUnliftIO safety" do
    it "with evalState" do
        x <- runUnliftIO do
            m <- evalStateIORef @Int 0 do
                withRunInIO \run -> do
                    pure $ run $ get @Int
            liftIO m
        x `shouldBe` 0
