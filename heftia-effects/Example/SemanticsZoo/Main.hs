-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Main where

import Control.Applicative ((<|>))
import Control.Arrow ((>>>))
import Control.Effect.ExtensibleChurch ((:!!))
import Control.Effect.Handler.Heftia.Except (runCatch, runThrow)
import Control.Effect.Handler.Heftia.NonDet (runChooseH, runNonDet)
import Control.Effect.Handler.Heftia.State (evalState)
import Control.Effect.Handler.Heftia.Writer (elaborateWriterPre, runTell)
import Control.Effect.Hefty (runPure, type ($))
import Data.Effect.Except (Catch, Throw, catch, throw)
import Data.Effect.NonDet (ChooseH, Empty)
import Data.Effect.State (State, get, put)
import Data.Effect.Writer (Tell, WriterH, listen, tell)
import Data.Function ((&))
import Data.Functor (($>))
import Data.Hefty.Extensible (type (<<|), type (<|))
import Data.Monoid (Sum (Sum))

statePlusExcept :: IO ()
statePlusExcept = do
    let action :: (State Bool <| ef, Throw () <| ef, Catch () <<| eh) => (eh :!! ef) Bool
        action = do
            (put True *> throw ()) `catch` \() -> pure ()
            get

    putStr "[ action & runCatch & runThrow & evalState ]: "
    print . runPure $ action & (runCatch @() >>> runThrow @() >>> evalState False)
    putStr "[ action & runCatch & evalState & runThrow ]: "
    print . runPure $ action & (runCatch @() >>> evalState False >>> runThrow @())

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
            putStr $ "[ " <> name <> " & runChooseH & runCatch & runNonDet & runThrow ]: "
            print . runPure $
                action & runChooseH & runCatch @() & runNonDet @[] & runThrow @()

            putStr $ "[ " <> name <> " & runChooseH & runCatch & runThrow & runNonDet ]: "
            print . runPure $
                action & runChooseH & runCatch @() & runThrow @() & runNonDet @[]

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

    putStr "[ action & runChooseH & elaborateWriterPre & runTell & runNonDet ]: "
    print . map (\(Sum m, (Sum n, b)) -> (m, (n, b))) . runPure $
        action
            & runChooseH
            & elaborateWriterPre @(Sum Int)
            & runTell @(Sum Int)
            & runNonDet @[]

    putStr "[ action & runChooseH & elaborateWriterPre & runNonDet & runTell ]: "
    print . (\(Sum m, xs) -> (m, map (\(Sum n, b) -> (n, b)) xs)) . runPure $
        action
            & runChooseH
            & elaborateWriterPre @(Sum Int)
            & runNonDet @[]
            & runTell @(Sum Int)

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
[ action & runCatch & runThrow & evalState ]: Right True
[ action & runCatch & evalState & runThrow ]: Right True

# NonDet + Except
[ action1 & runChooseH & runCatch & runNonDet & runThrow ]: Right [True,False]
[ action1 & runChooseH & runCatch & runThrow & runNonDet ]: [Right True,Right False]
[ action2 & runChooseH & runCatch & runNonDet & runThrow ]: Right [False,True]
[ action2 & runChooseH & runCatch & runThrow & runNonDet ]: [Right False,Right True]

# NonDet + Writer
[ action & runChooseH & elaborateWriterPre & runTell & runNonDet ]: [(3,(3,True)),(4,(4,False))]
[ action & runChooseH & elaborateWriterPre & runNonDet & runTell ]: (6,[(3,True),(4,False)])

[Note] All other permutations will cause type errors.
-}
