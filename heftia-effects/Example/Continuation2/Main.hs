-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Main where

import Control.Effect (sendIns)
import Control.Effect.ExtensibleChurch (runEff, type (!!))
import Control.Effect.Handler.Heftia.Cont (Shift, Shift_, getCC, getCC_, runShift, runShift_)
import Control.Effect.Handler.Heftia.Reader (elaborateLocal, interpretAsk)
import Control.Effect.Handler.Heftia.State (evalState)
import Control.Effect.Hefty (interpretH, send1, type ($))
import Control.Monad.Extra (whenM)
import Data.Effect.HFunctor ((:+:))
import Data.Effect.Reader (Ask, Local, ask, local)
import Data.Effect.State (State, get, modify)
import Data.Free.Sum (type (+))
import Data.Function ((&))
import Data.Functor ((<&>))

main :: IO ()
main = do
    putStrLn "[elaborateLocalThenShift]"
    elaborateLocalThenShift

    putStrLn ""
    putStrLn "[elaborateShiftThenLocal]"
    elaborateShiftThenLocal

{-
===== result =====

[elaborateLocalThenShift]
[local scope outer] env = 1.0
[local scope inner] env = 2.0
[local scope outer] env = 1.0
[local scope inner] env = 2.0
[local scope outer] env = 1.0
[local scope inner] env = 2.0
[local scope outer] env = 1.0
[local scope inner] env = 2.0
[local scope outer] env = 1.0
[local scope inner] env = 2.0
[local scope outer] env = 1.0

[elaborateShiftThenLocal]
[local scope outer] env = 1.0
[local scope inner] env = 2.0
[local scope outer] env = 2.0
[local scope inner] env = 4.0
[local scope outer] env = 4.0
[local scope inner] env = 8.0
[local scope outer] env = 8.0
[local scope inner] env = 16.0
[local scope outer] env = 16.0
[local scope inner] env = 32.0
[local scope outer] env = 32.0

-}

elaborateLocalThenShift :: IO ()
elaborateLocalThenShift =
    prog
        & interpretH elaborateLocal
        & interpretAsk 1.0
        & runEff
        & runShift
        & evalState 0
        & runEff
  where
    prog :: Local Double !! Ask Double + Shift () !! State Int + IO $ ()
    prog = do
        k <- send1 getCC
        env <- ask @Double
        send1 $ sendIns $ putStrLn $ "[local scope outer] env = " ++ show env
        local @Double (* 2) do
            whenM (send1 (get @Int) <&> (< 5)) do
                send1 $ modify @Int (+ 1)
                env' <- ask @Double
                send1 $ sendIns $ putStrLn $ "[local scope inner] env = " ++ show env'
                send1 k

elaborateShiftThenLocal :: IO ()
elaborateShiftThenLocal = do
    prog
        & runShift_
        & interpretH elaborateLocal
        & interpretAsk 1.0
        & evalState 0
        & runEff
  where
    prog :: Shift_ :+: Local Double !! Ask Double + State Int + IO $ ()
    prog = do
        k <- getCC_
        env <- ask @Double
        sendIns $ putStrLn $ "[local scope outer] env = " ++ show env
        local @Double (* 2) do
            whenM (get @Int <&> (< 5)) do
                modify @Int (+ 1)
                env' <- ask @Double
                sendIns $ putStrLn $ "[local scope inner] env = " ++ show env'
                k
