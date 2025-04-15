{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}

-- SPDX-License-Identifier: MPL-2.0

module Test.Semantics where

import Control.Applicative ((<|>))
import Control.Effect (type (~>))
import Control.Monad.Hefty (
    Eff,
    Effect,
    interpret,
    runPure,
    (:>),
 )
import Control.Monad.Hefty.Except (runCatch, runThrow)
import Control.Monad.Hefty.NonDet (runChooseH, runNonDet)
import Control.Monad.Hefty.State (evalState)
import Control.Monad.Hefty.Writer (runTell, runWriterHPre)
import Data.Effect.Except (Catch, Throw, catch, throw)
import Data.Effect.NonDet (ChooseH, Empty)
import Data.Effect.State (State, get, put)
import Data.Effect.TH (makeEffectF)
import Data.Effect.Writer (Tell, WriterH, listen, tell)
import Data.Functor (($>))
import Data.Monoid (Sum (Sum))
import Test.Hspec (Spec, describe, it, shouldBe)

spec_State_Except :: Spec
spec_State_Except = describe "State & Except semantics" do
    let action :: (State Bool :> es, Throw () :> es, Catch () :> es) => Eff es Bool
        action = do
            (put True *> throw ()) `catch` \() -> pure ()
            get

    it "evalState . runThrow  $ (put True *> throw) `catch` ()  ==>  Right True" do
        runPure (evalState False . runThrow @() . runCatch @() $ action) `shouldBe` Right True
    it "runThrow  . evalState $ (put True *> throw) `catch` ()  ==>  Right True" do
        runPure (runThrow @() . evalState False . runCatch @() $ action) `shouldBe` Right True

spec_NonDet_Except :: Spec
spec_NonDet_Except = describe "NonDet & Except semantics" do
    let action1
            , action2
                :: (Empty :> es, ChooseH :> es, Throw () :> es, Catch () :> es) => Eff es Bool
        action1 = (pure True <|> throw ()) `catch` \() -> pure False
        action2 = (throw () <|> pure True) `catch` \() -> pure False

    it "runNonDet . runThrow  $ (True <|> throw) `catch` False  ==>  [Right True, Right False]" do
        runPure (runNonDet @[] . runThrow @() . runCatch @() . runChooseH $ action1) `shouldBe` [Right True, Right False]
    it "runThrow  . runNonDet $ (True <|> throw) `catch` False  ==>  Right [True, False]" do
        runPure (runThrow @() . runNonDet @[] . runCatch @() . runChooseH $ action1) `shouldBe` Right [True, False]
    it "runNonDet . runThrow  $ (throw <|> True) `catch` False  ==>  [Right False, Right False]" do
        runPure (runNonDet @[] . runThrow @() . runCatch @() . runChooseH $ action2) `shouldBe` [Right False, Right True]
    it "runThrow  . runNonDet $ (throw <|> True) `catch` False  ==>  Right [False, True]" do
        runPure (runThrow @() . runNonDet @[] . runCatch @() . runChooseH $ action2) `shouldBe` Right [False, True]

spec_NonDet_Writer :: Spec
spec_NonDet_Writer = describe "NonDet & Writer semantics" do
    let action
            :: (Empty :> es, ChooseH :> es, Tell (Sum Int) :> es, WriterH (Sum Int) :> es)
            => Eff es (Sum Int, Bool)
        action = listen $ add 1 *> (add 2 $> True <|> add 3 $> False)
          where
            add = tell . Sum @Int

    it "runNonDet . runTell   $ listen $ add 1 *> (add 2 $> True <|> add 3 $> False)  ==>  [(3, (3, True)), (4, (4, False))]" do
        runPure (runNonDet @[] . runTell @(Sum Int) . runWriterHPre @(Sum Int) . runChooseH $ action) `shouldBe` [(3, (3, True)), (4, (4, False))]
    it "runTell   . runNonDet $ listen $ add 1 *> (add 2 $> True <|> add 3 $> False)  ==>  (6, [(3, True), (4, False)])" do
        runPure (runTell @(Sum Int) . runNonDet @[] . runWriterHPre @(Sum Int) . runChooseH $ action) `shouldBe` (6, [(3, True), (4, False)])

data SomeEff :: Effect where
    SomeAction :: SomeEff f String
makeEffectF ''SomeEff

spec_The_issue_12 :: Spec
spec_The_issue_12 = describe "hasura/eff#12 semantics" do
    let action :: (Catch String :> es, SomeEff :> es) => Eff es String
        action = someAction `catch` \(_ :: String) -> pure "caught"

        runSomeEff :: (Throw String :> es) => Eff (SomeEff ': es) ~> Eff es
        runSomeEff = interpret (\SomeAction -> throw "not caught")

    it "runCatch . interpret (\\SomeAction -> throw \"not caught\") $ someAction `catch` \"caught\"  ==>  Right \"caught\"" do
        runPure (runThrow @String . runCatch @String . runSomeEff $ action) `shouldBe` Right "caught"
    it "interpret (\\SomeAction -> throw \"not caught\") . runCatch $ someAction `catch` \"caught\"  ==>  Left \"not caught\"" do
        runPure (runThrow @String . runSomeEff . runCatch @String $ action) `shouldBe` Left "not caught"
