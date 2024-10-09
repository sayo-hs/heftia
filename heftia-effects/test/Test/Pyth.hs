-- SPDX-License-Identifier: MPL-2.0

module Test.Pyth where

import Control.Applicative (empty, (<|>))
import Control.Monad (MonadPlus)
import Control.Monad.Hefty.Interpret (runPure)
import Control.Monad.Hefty.NonDet (runChooseH, runNonDet)
import Test.Hspec (Spec, describe, it, shouldBe)

search :: (MonadPlus m) => Int -> m (Int, Int, Int)
search upbound = do
    x <- choice upbound
    y <- choice upbound
    z <- choice upbound
    if x * x + y * y == z * z then return (x, y, z) else empty
  where
    choice 0 = empty
    choice n = choice (n - 1) <|> pure n

spec_Pyth :: Spec
spec_Pyth = describe "Non-Deterministic Search for Pythagorean triangle numbers" do
    it "n = 16" do
        (runPure . runNonDet . runChooseH $ search 16)
            `shouldBe` [ (3, 4, 5)
                       , (4, 3, 5)
                       , (5, 12, 13)
                       , (6, 8, 10)
                       , (8, 6, 10)
                       , (9, 12, 15)
                       , (12, 5, 13)
                       , (12, 9, 15)
                       ]
