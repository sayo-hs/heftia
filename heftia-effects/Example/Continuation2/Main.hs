-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Main where

import Control.Arrow ((>>>))
import Control.Effect.Interpreter.Heftia.Reader (runAsk, runLocal)
import Control.Effect.Interpreter.Heftia.ShiftReset (evalShift, runShift_)
import Control.Effect.Interpreter.Heftia.State (evalState)
import Control.Effect.Key (key)
import Control.Monad.Extra (whenM)
import Control.Monad.Hefty.Interpret (runEff)
import Control.Monad.Hefty.Transform (raise, unkey)
import Control.Monad.Hefty.Types (Eff, send, send0)
import Control.Monad.IO.Class (liftIO)
import Data.Effect.Key (type (#>))
import Data.Effect.Reader (Ask, Local, ask, local)
import Data.Effect.ShiftReset (Shift, Shift_, getCC, getCC_)
import Data.Effect.State (State, get'', modify)
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
        & runLocal
        & runAsk 1
        & runEff
        & evalShift
        & (unkey >>> evalState 0)
        & runEff
  where
    prog :: Eff '[Local Int] '[Ask Int, Eff '[Shift ()] '["counter" #> State Int, IO]] ()
    prog = do
        k <- raise $ send0 getCC
        env <- ask @Int
        raise $ send0 $ liftIO $ putStrLn $ "[local scope outer] env = " ++ show env
        local @Int (* 2) do
            whenM (raise $ send0 (get'' @"counter") <&> (< 5)) do
                raise $ send0 $ modify (+ 1) & key @"counter"
                env' <- ask @Int
                raise $ send0 $ liftIO $ putStrLn $ "[local scope inner] env = " ++ show env'
                send k

handleShiftThenReader :: IO ()
handleShiftThenReader = do
    prog
        & runShift_
        & runLocal
        & runAsk 1
        & (unkey >>> evalState 0)
        & runEff
  where
    prog :: Eff '[Shift_, Local Int] '[Ask Int, "counter" #> State Int, IO] ()
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
