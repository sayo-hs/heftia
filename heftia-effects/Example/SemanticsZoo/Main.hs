-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
[lexi-lambda's semantics-zoo.md](https://github.com/lexi-lambda/eff/blob/master/notes/semantics-zoo.md).

It can be confirmed that Heftia also realizes continuation-based semantics equivalent to eff.
-}
module Main where

import Control.Applicative ((<|>))
import Control.Effect.ExtensibleChurch ((:!!))
import Control.Effect.Hefty (runPure, type ($))
import Control.Effect.Interpreter.Heftia.Except (runCatch, runThrow)
import Control.Effect.Interpreter.Heftia.NonDet (runChooseH, runNonDet)
import Control.Effect.Interpreter.Heftia.State (evalState)
import Control.Effect.Interpreter.Heftia.Writer (elaborateWriterPre, runTell)
import Data.Effect.Except (Catch, Throw, catch, throw)
import Data.Effect.NonDet (ChooseH, Empty)
import Data.Effect.State (State, get, put)
import Data.Effect.Writer (Tell, WriterH, listen, tell)
import Data.Functor (($>))
import Data.Hefty.Extensible (type (<<|), type (<|))
import Data.Monoid (Sum (Sum))

statePlusExcept :: IO ()
statePlusExcept = do
    let action :: (State Bool <| ef, Throw () <| ef, Catch () <<| eh) => (eh :!! ef) Bool
        action = do
            (put True *> throw ()) `catch` \() -> pure ()
            get

    putStr "( evalState . runThrow . runCatch $ action ) = "
    print . runPure $ evalState False . runThrow @() . runCatch @() $ action
    putStr "( runThrow . evalState . runCatch $ action ) = "
    print . runPure $ runThrow @() . evalState False . runCatch @() $ action

nonDetPlusExcept :: IO ()
nonDetPlusExcept = do
    let action1
            , action2 ::
                (Empty <| ef, ChooseH <<| eh, Throw () <| ef, Catch () <<| eh) => eh :!! ef $ Bool
        action1 = (pure True <|> throw ()) `catch` \() -> pure False
        action2 = (throw () <|> pure True) `catch` \() -> pure False

        testAllPattern ::
            ( forall eh ef.
              (Empty <| ef, ChooseH <<| eh, Throw () <| ef, Catch () <<| eh) =>
              (eh :!! ef) Bool
            ) ->
            String ->
            IO ()
        testAllPattern action name = do
            putStr $ "( runNonDet . runThrow . runCatch . runChooseH $ " <> name <> " ) = "
            print . runPure $
                runNonDet @[] . runThrow @() . runCatch @() . runChooseH $ action

            putStr $ "( runThrow . runNonDet . runCatch . runChooseH $ " <> name <> " ) = "
            print . runPure $
                runThrow @() . runNonDet @[] . runCatch @() . runChooseH $ action

    testAllPattern action1 "action1"
    testAllPattern action2 "action2"

nonDetPlusWriter :: IO ()
nonDetPlusWriter = do
    let action ::
            (Empty <| ef, ChooseH <<| eh, Tell (Sum Int) <| ef, WriterH (Sum Int) <<| eh) =>
            eh :!! ef $ (Sum Int, Bool)
        action = listen $ add 1 *> (add 2 $> True <|> add 3 $> False)
          where
            add = tell . Sum @Int

    putStr "( runNonDet . runTell . elaborateWriter . runChooseH $ action ) = "
    print . map (\(Sum m, (Sum n, b)) -> (m, (n, b))) . runPure $
        runNonDet @[] . runTell @(Sum Int) . elaborateWriterPre @(Sum Int) . runChooseH $ action

    putStr "( runTell . runNonDet . elaborateWriter . runChooseH $ action ) = "
    print . (\(Sum m, xs) -> (m, map (\(Sum n, b) -> (n, b)) xs)) . runPure $
        runTell @(Sum Int) . runNonDet @[] . elaborateWriterPre @(Sum Int) . runChooseH $ action

main :: IO ()
main = do
    putStrLn "# State + Except"
    statePlusExcept

    putStrLn "\n# NonDet + Except"
    nonDetPlusExcept

    putStrLn "\n# NonDet + Writer"
    nonDetPlusWriter

    putStrLn "\n[Note] All other permutations will cause type errors."

{-
# State + Except
( evalState . runThrow . runCatch $ action ) = Right True
( runThrow . evalState . runCatch $ action ) = Right True

# NonDet + Except
( runNonDet . runThrow . runCatch . runChooseH $ action1 ) = [Right True,Right False]
( runThrow . runNonDet . runCatch . runChooseH $ action1 ) = Right [True,False]
( runNonDet . runThrow . runCatch . runChooseH $ action2 ) = [Right False,Right True]
( runThrow . runNonDet . runCatch . runChooseH $ action2 ) = Right [False,True]

# NonDet + Writer
( runNonDet . runTell . elaborateWriter . runChooseH $ action ) = [(3,(3,True)),(4,(4,False))]
( runTell . runNonDet . elaborateWriter . runChooseH $ action ) = (6,[(3,True),(4,False)])

[Note] All other permutations will cause type errors.
-}
