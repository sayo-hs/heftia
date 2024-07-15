-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Main where

import Control.Arrow ((>>>))
import Control.Effect.ExtensibleChurch (runEff, type (!!))
import Control.Effect.Handler.Heftia.Reader (elaborateLocal, interpretAsk)
import Control.Effect.Handler.Heftia.ShiftReset (runShift, runShift_)
import Control.Effect.Handler.Heftia.State (evalState)
import Control.Effect.Hefty (interpretH, send1, unkeyEff, type ($))
import Control.Effect.Key (key)
import Control.Monad.Extra (whenM)
import Control.Monad.IO.Class (liftIO)
import Data.Effect.HFunctor ((:+:))
import Data.Effect.Key (type (#>))
import Data.Effect.Reader (Ask, Local, ask, local)
import Data.Effect.ShiftReset (Shift, Shift_, getCC, getCC_)
import Data.Effect.State (State, get, get'', modify)
import Data.Free.Sum (type (+))
import Data.Function ((&))
import Data.Functor ((<&>))

main :: IO ()
main = do
    putStrLn "[handleReaderThenShift]"
    handleReaderThenShift

    putStrLn ""
    putStrLn "[handleShiftThenReader]"
    handleShiftThenReader

{-
===== result =====

[handleReaderThenShift]
[local scope outer] env = 1
[local scope inner] env = 2
[local scope outer] env = 1
[local scope inner] env = 2
[local scope outer] env = 1
[local scope inner] env = 2
[local scope outer] env = 1
[local scope inner] env = 2
[local scope outer] env = 1
[local scope inner] env = 2
[local scope outer] env = 1

[handleShiftThenReader]
[local scope outer] env = 1
[local scope inner] env = 2
[local scope outer] env = 2
[local scope inner] env = 4
[local scope outer] env = 4
[local scope inner] env = 8
[local scope outer] env = 8
[local scope inner] env = 16
[local scope outer] env = 16
[local scope inner] env = 32
[local scope outer] env = 32
-}

handleReaderThenShift :: IO ()
handleReaderThenShift =
    prog
        & interpretH elaborateLocal
        & interpretAsk 1
        & runEff
        & runShift
        & (unkeyEff >>> evalState 0)
        & runEff
  where
    prog :: Local Int !! Ask Int + Shift () !! "counter" #> State Int + IO $ ()
    prog = do
        k <- send1 getCC
        env <- ask @Int
        send1 $ liftIO $ putStrLn $ "[local scope outer] env = " ++ show env
        local @Int (* 2) do
            whenM (send1 (get'' @"counter") <&> (< 5)) do
                send1 $ modify (+ 1) & key @"counter"
                env' <- ask @Int
                send1 $ liftIO $ putStrLn $ "[local scope inner] env = " ++ show env'
                send1 k

handleShiftThenReader :: IO ()
handleShiftThenReader = do
    prog
        & runShift_
        & interpretH elaborateLocal
        & interpretAsk 1
        & (unkeyEff >>> evalState 0)
        & runEff
  where
    prog :: Shift_ :+: Local Int !! Ask Int + "counter" #> State Int + IO $ ()
    prog = do
        k <- getCC_
        env <- ask @Int
        liftIO $ putStrLn $ "[local scope outer] env = " ++ show env
        local @Int (* 2) do
            whenM (get'' @"counter" <&> (< 5)) do
                modify (+ 1) & key @"counter"
                env' <- ask @Int
                liftIO $ putStrLn $ "[local scope inner] env = " ++ show env'
                k
