{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

-- SPDX-License-Identifier: MPL-2.0

module Test.Async where

import Control.Monad.Hefty (Eff, liftIO, type (<|))
import Control.Monad.Hefty.Concurrent.Async (
    Future (await, cancel, poll),
    HasAsync,
    async,
    perform,
    runAsyncIO,
    runAsyncSeq,
 )
import Control.Monad.Hefty.Concurrent.Parallel (halt, runHaltIO)
import Control.Monad.Hefty.Concurrent.Timer (runTimerIO)
import Control.Monad.Hefty.State (get, modify, runStateIORef)
import Control.Monad.Hefty.Unlift (runUnliftIO)
import Data.Effect.Concurrent.Timer (sleep)
import Data.Functor ((<&>))
import Test.Hspec (Spec, it, shouldBe, shouldReturn, shouldThrow)
import UnliftIO (throwIO)

spec_Async :: Spec
spec_Async = do
    let
        prog :: (HasAsync ef f, IO <| ef) => Eff eh ef (String, String)
        prog = runTimerIO . runStateIORef "" $ do
            fu <- async do
                sleep 0.001
                modify (<> "B")
                get @String <&> (<> "C")
            modify (<> "A")
            perform $ await fu

    it "sequential" do
        (s, a) <- runUnliftIO . runAsyncSeq $ prog
        s `shouldBe` "BA"
        a `shouldBe` "BC"

    it "await" do
        (s, a) <- runUnliftIO . runAsyncIO $ prog
        s `shouldBe` "AB"
        a `shouldBe` "ABC"

    it "cancel" do
        (s, ()) <- runUnliftIO . runTimerIO . runStateIORef "" . runAsyncIO $ do
            fu <- async do
                sleep 0.001
                modify (<> "B")
            modify (<> "A")
            perform $ cancel fu
            sleep 0.002
        s `shouldBe` "A"

    it "poll" do
        (s, ()) <- runUnliftIO . runTimerIO . runStateIORef "" . runAsyncIO $ do
            fu <- async do
                sleep 0.001
                modify (<> "B")
                get @String <&> (<> "C")
            modify (<> "A")

            x <- perform $ poll fu
            liftIO $ x `shouldBe` Nothing

            sleep 0.002

            x' <- perform $ poll fu
            liftIO $ x' `shouldBe` Just "ABC"

        s `shouldBe` "AB"

    it "catch IO exception" $
        do
            runUnliftIO . runAsyncIO $ do
                fu <- async $ throwIO $ userError "Test IO Exception"
                perform $ await fu
            `shouldThrow` (== userError "Test IO Exception")

    it "suppress IO exception" $
        do
            runUnliftIO . runTimerIO . runAsyncIO $ do
                _ <- async $ throwIO $ userError "Test IO Exception"
                sleep 0.001
                pure ()
            `shouldReturn` ()

    it "early exit" $
        do
            runUnliftIO . runAsyncIO . runHaltIO $ do
                _ <- async halt
                pure ()
            `shouldReturn` ()
