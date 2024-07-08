{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Main where

import Control.Effect (type (~>))
import Control.Effect.ExtensibleChurch (runEff, type (:!!))
import Control.Effect.Hefty (interposeRec, interpretRec, unkeyEff)
import Control.Effect.Key (SendInsBy)
import Control.Monad.IO.Class (liftIO)
import Data.Effect.Key (unKey, type (#>))
import Data.Effect.TH (makeEffectF)
import Data.Hefty.Extensible (ForallHFunctor, MemberBy, type (<|))

data Teletype a where
    ReadTTY :: Teletype String
    WriteTTY :: String -> Teletype ()

makeEffectF [''Teletype]

teletypeToIO :: (IO <| r, ForallHFunctor eh) => eh :!! LTeletype ': r ~> eh :!! r
teletypeToIO = interpretRec \case
    ReadTTY -> liftIO getLine
    WriteTTY msg -> liftIO $ putStrLn msg

echo :: (SendInsBy "tty1" Teletype m, Monad m) => m ()
echo = do
    i <- readTTY'' @"tty1"
    case i of
        "" -> pure ()
        _ -> writeTTY'' @"tty1" i >> echo

strong :: (MemberBy "tty1" Teletype ef, ForallHFunctor eh) => eh :!! ef ~> eh :!! ef
strong =
    interposeRec @("tty1" #> _) \e -> case unKey e of
        ReadTTY -> readTTY'' @"tty1"
        WriteTTY msg -> writeTTY'' @"tty1" $ msg <> "!"

main :: IO ()
main = runEff do
    liftIO $ putStrLn "Please enter something..."
    teletypeToIO . unkeyEff @"tty1" . strong . strong $ echo
