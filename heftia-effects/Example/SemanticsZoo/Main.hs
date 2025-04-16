{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}

-- SPDX-License-Identifier: MPL-2.0

{- |
[lexi-lambda's semantics-zoo.md](https://github.com/lexi-lambda/eff/blob/master/notes/semantics-zoo.md).

It can be confirmed that Heftia also realizes continuation-based semantics equivalent to eff.
-}
module Main where

import Control.Applicative ((<|>))
import Control.Monad.Hefty (
    Eff,
    Effect,
    interpret,
    makeEffectF,
    runPure,
    type (:>),
    type (~>),
 )
import Control.Monad.Hefty.Except (runCatch, runThrow)
import Control.Monad.Hefty.NonDet (runChooseH, runNonDet)
import Control.Monad.Hefty.State (evalState)
import Control.Monad.Hefty.Writer (runTell, runWriterHPre)
import Data.Effect.Except (Catch, Throw, catch, throw)
import Data.Effect.NonDet (ChooseH, Empty)
import Data.Effect.State (State, get, put)
import Data.Effect.Writer (Tell, WriterH, listen, tell)
import Data.Functor (($>))
import Data.Monoid (Sum (Sum))

statePlusExcept :: IO ()
statePlusExcept = do
    let action :: (State Bool :> es, Throw () :> es, Catch () :> es) => Eff es Bool
        action = do
            (put True *> throw ()) `catch` \() -> pure ()
            get

    putStr "( evalState . runThrow . runCatch $ action ) = "
    print . runPure $ evalState False . runThrow @() . runCatch @() $ action
    putStr "( runThrow . evalState . runCatch $ action ) = "
    print . runPure $ runThrow @() . evalState False . runCatch @() $ action

nonDetPlusExcept :: IO ()
nonDetPlusExcept = do
    let action1, action2 :: (Empty :> es, ChooseH :> es, Throw () :> es, Catch () :> es) => Eff es Bool
        action1 = (pure True <|> throw ()) `catch` \() -> pure False
        action2 = (throw () <|> pure True) `catch` \() -> pure False

        testAllPattern
            :: ( forall es
                  . (Empty :> es, ChooseH :> es, Throw () :> es, Catch () :> es)
                 => Eff es Bool
               )
            -> String
            -> IO ()
        testAllPattern action name = do
            putStr $ "( runNonDet . runThrow . runCatch . runChooseH $ " <> name <> " ) = "
            print . runPure $
                runNonDet @[] . runThrow @() . runCatch @() . runChooseH $
                    action

            putStr $ "( runThrow . runNonDet . runCatch . runChooseH $ " <> name <> " ) = "
            print . runPure $
                runThrow @() . runNonDet @[] . runCatch @() . runChooseH $
                    action

    testAllPattern action1 "action1"
    testAllPattern action2 "action2"

nonDetPlusWriter :: IO ()
nonDetPlusWriter = do
    let action
            :: (Empty :> es, ChooseH :> es, Tell (Sum Int) :> es, WriterH (Sum Int) :> es)
            => Eff es (Sum Int, Bool)
        action = listen $ add 1 *> (add 2 $> True <|> add 3 $> False)
          where
            add = tell . Sum @Int

    putStr "( runNonDet . runTell . runWriterH . runChooseH $ action ) = "
    print . map (\(Sum m, (Sum n, b)) -> (m, (n, b))) . runPure $
        runNonDet @[] . runTell @(Sum Int) . runWriterHPre @(Sum Int) . runChooseH $
            action

    putStr "( runTell . runNonDet . runWriterH . runChooseH $ action ) = "
    print . (\(Sum m, xs) -> (m, map (\(Sum n, b) -> (n, b)) xs)) . runPure $
        runTell @(Sum Int) . runNonDet @[] . runWriterHPre @(Sum Int) . runChooseH $
            action

data SomeEff :: Effect where
    SomeAction :: SomeEff f String
makeEffectF ''SomeEff

theIssue12 :: IO ()
theIssue12 = do
    let action :: (Catch String :> es, SomeEff :> es) => Eff es String
        action = someAction `catch` \(_ :: String) -> pure "caught"

        runSomeEff :: (Throw String :> es) => Eff (SomeEff ': es) ~> Eff es
        runSomeEff = interpret \SomeAction -> throw "not caught"

    putStr "interpret SomeEff then runCatch : ( runThrow . runCatch . runSomeEff $ action ) = "
    print $ runPure $ runThrow @String . runCatch @String . runSomeEff $ action

    putStr "runCatch then interpret SomeEff : ( runThrow . runSomeEff . runCatch $ action ) = "
    print $ runPure $ runThrow @String . runSomeEff . runCatch @String $ action

main :: IO ()
main = do
    putStrLn "# State & Except"
    statePlusExcept

    putStrLn "\n# NonDet & Except"
    nonDetPlusExcept

    putStrLn "\n# NonDet & Writer"
    nonDetPlusWriter

    putStrLn "\n# https://github.com/hasura/eff/issues/12"
    theIssue12

    putStrLn "\n[Note] All other permutations will cause type errors."

{-
# State & Except
( evalState . runThrow . runCatch $ action ) = Right True
( runThrow . evalState . runCatch $ action ) = Right True

# NonDet & Except
( runNonDet . runThrow . runCatch . runChooseH $ action1 ) = [Right True,Right False]
( runThrow . runNonDet . runCatch . runChooseH $ action1 ) = Right [True,False]
( runNonDet . runThrow . runCatch . runChooseH $ action2 ) = [Right False,Right True]
( runThrow . runNonDet . runCatch . runChooseH $ action2 ) = Right [False,True]

# NonDet & Writer
( runNonDet . runTell . runWriterH . runChooseH $ action ) = [(3,(3,True)),(4,(4,False))]
( runTell . runNonDet . runWriterH . runChooseH $ action ) = (6,[(3,True),(4,False)])

# https://github.com/hasura/eff/issues/12
interpret SomeEff then runCatch : ( runThrow . runCatch . runSomeEff $ action ) = Right "caught"
runCatch then interpret SomeEff : ( runThrow . runSomeEff . runCatch $ action ) = Left "not caught"

[Note] All other permutations will cause type errors.
-}
