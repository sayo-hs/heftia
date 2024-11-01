-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Main where

import Control.Effect.Key (key)
import Control.Monad.Extra (whenM)
import Control.Monad.Hefty (
    Eff,
    liftIO,
    raiseH,
    runEff,
    send,
    sendN,
    unkey,
    (&),
    type (!!),
    type ($),
    type (+),
    type (:+:),
 )
import Control.Monad.Hefty.Reader (runReader)
import Control.Monad.Hefty.ShiftReset (Shift, ShiftEff (ShiftEff), evalShift, runShift_)
import Control.Monad.Hefty.State (evalState)
import Data.Effect.Key (type (#>))
import Data.Effect.Reader (Ask, Local, ask, local)
import Data.Effect.ShiftReset (Shift_, getCC, getCC_)
import Data.Effect.State (State, get'', modify)
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
        & runReader 1
        & runEff
        & evalShift
        & (evalState 0 . unkey)
        & runEff
  where
    prog
        :: (r ~ '["counter" #> State Int, IO])
        => Eff '[Local Int] '[Ask Int, Eff '[Shift () '[] r] r] ()
    prog = do
        ShiftEff k <- sendN @1 $ getCC
        env <- ask @Int
        sendN @1 $ liftIO $ putStrLn $ "[local scope outer] env = " ++ show env
        local @Int (* 2) do
            whenM (sendN @1 (get'' @"counter") <&> (< 5)) do
                sendN @1 $ modify (+ 1) & key @"counter"
                env' <- ask @Int
                sendN @1 $ liftIO $ putStrLn $ "[local scope inner] env = " ++ show env'
                send k

handleShiftThenReader :: IO ()
handleShiftThenReader = do
    prog
        & runShift_
        & runReader 1
        & (evalState 0 . unkey)
        & runEff
  where
    prog
        :: (r ~ (Ask Int + "counter" #> State Int + IO))
        => Shift_ (Local Int !! r) :+: Local Int !! r $ ()
    prog = do
        k <- getCC_
        env <- ask @Int
        liftIO $ putStrLn $ "[local scope outer] env = " ++ show env
        local @Int (* 2) do
            whenM (get'' @"counter" <&> (< 5)) do
                modify (+ 1) & key @"counter"
                env' <- ask @Int
                liftIO $ putStrLn $ "[local scope inner] env = " ++ show env'
                raiseH k
