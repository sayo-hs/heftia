-- SPDX-License-Identifier: MPL-2.0

module Test.Coroutine where

import Control.Effect.Interpreter.Heftia.Coroutine (runCoroutine)
import Control.Monad
import Control.Monad.Hefty
import Data.Effect.Coroutine
import Test.Hspec

generateSeq :: (Yield Int Int <| ef) => Int -> Eff '[] ef [Int]
generateSeq n =
    forM [1 .. n] yield

replyDouble :: Status (Eff '[] ef) Int Int r -> Eff '[] ef r
replyDouble = \case
    Done r -> pure r
    Continue i f -> replyDouble =<< f (i * 2)

spec_Coroutine :: Spec
spec_Coroutine = it "n = 5" do
    runPure (replyDouble =<< runCoroutine (generateSeq 5)) `shouldBe` [2, 4, 6, 8, 10]
