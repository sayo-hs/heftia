-- SPDX-License-Identifier: MPL-2.0

module Test.Coroutine where

import Control.Monad (forM)
import Control.Monad.Hefty ((:>))
import Control.Monad.Hefty.Coroutine (runCoroutine)
import Control.Monad.Hefty.Interpret (runPure)
import Control.Monad.Hefty.Types (Eff)
import Data.Effect.Coroutine (Status (..), Yield, yield)
import Test.Hspec (Spec, it, shouldBe)

generateSeq :: (Yield Int Int :> es) => Int -> Eff es [Int]
generateSeq n =
    forM [1 .. n] yield

replyDouble :: Status (Eff es) Int Int r -> Eff es r
replyDouble = \case
    Done r -> pure r
    Continue i f -> replyDouble =<< f (i * 2)

spec_Coroutine :: Spec
spec_Coroutine = it "Generator & Reply" do
    runPure (replyDouble =<< runCoroutine (generateSeq 5)) `shouldBe` [2, 4, 6, 8, 10]
