{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

-- SPDX-License-Identifier: MPL-2.0

module Test.Concurrent where

import Control.Applicative ((<|>))
import Control.Monad.Hefty (Eff, liftIO, runEff, type (<<|), type (<|))
import Control.Monad.Hefty.Concurrent.Parallel (
    Concurrently (Concurrently, runConcurrently),
    Parallel,
    cancels,
    input,
    polling,
    runConcurrentIO,
    runParallelAsSequential,
 )
import Control.Monad.Hefty.Concurrent.Timer (runTimerIO, sleep)
import Control.Monad.Hefty.State (get, modify, runStateIORef)
import Control.Monad.Hefty.Unlift (runUnliftIO)
import Data.Functor ((<&>))
import Test.Hspec (Spec, it, shouldBe)

spec_Concurrent :: Spec
spec_Concurrent = do
    let
        prog :: (Parallel <<| eh, IO <| ef) => Eff eh ef (String, String)
        prog = runTimerIO . runStateIORef "" . runConcurrently $ do
            r <- Concurrently do
                sleep 0.001
                modify (<> "B")
                get @String <&> (<> "C")
            Concurrently do
                modify (<> "A")
            pure r

    it "Parallel" do
        (s, a) <- runUnliftIO . runConcurrentIO $ prog
        s `shouldBe` "AB"
        a `shouldBe` "ABC"

    it "Sequential" do
        (s, a) <- runEff . runParallelAsSequential $ prog
        s `shouldBe` "BA"
        a `shouldBe` "BC"

    it "Race" do
        (s, a) <-
            runUnliftIO . runTimerIO . runStateIORef "" . runConcurrentIO . runConcurrently $
                let a = Concurrently do
                        modify (<> "A")
                        sleep 0.002
                        pure '1'

                    b = Concurrently do
                        sleep 0.001
                        modify (<> "B")
                        sleep 0.002
                        modify (<> "C")
                        pure '2'
                 in a <|> b

        s `shouldBe` "AB"
        a `shouldBe` '1'

    it "Cancel" do
        (s, ((), b)) <-
            runUnliftIO . runTimerIO . runStateIORef "" . runConcurrentIO $
                sleep 0.001 `cancels` do
                    modify (<> "A")
                    sleep 0.002
                    modify (<> "B")

        s `shouldBe` "A"
        b `shouldBe` Nothing

    it "Poll" do
        (s, ()) <- runUnliftIO . runTimerIO . runStateIORef "" . runConcurrentIO $ do
            polling
                do
                    sleep 0.001
                    modify (<> "B")
                    get @String <&> (<> "C")
                do
                    modify (<> "A")

                    x <- input
                    liftIO $ x `shouldBe` Nothing @String

                    sleep 0.002

                    x' <- input
                    liftIO $ x' `shouldBe` Just "ABC"

                    pure ()

        s `shouldBe` "AB"
