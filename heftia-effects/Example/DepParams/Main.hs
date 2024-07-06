{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Main where

import Control.Effect (SendIns (sendIns), type (~>))
import Control.Effect.ExtensibleChurch (runEff, type (:!!))
import Control.Effect.Hefty (interposeRec, interpretRec, unkeyEff)
import Data.Effect.HFunctor (HFunctor)
import Data.Effect.TH (makeEffectF)
import Data.Hefty.Extensible (Forall, type (<|), MemberBy)
import Data.Effect.Key (type (#>), unKey)
import Data.Function ((&))
import Control.Effect.Key (key, SendInsBy)

data Teletype a where
    ReadTTY :: Teletype String
    WriteTTY :: String -> Teletype ()

makeEffectF [''Teletype]

teletypeToIO :: (IO <| r, Forall HFunctor eh) => eh :!! LTeletype ': r ~> eh :!! r
teletypeToIO = interpretRec \case
    ReadTTY -> sendIns getLine
    WriteTTY msg -> sendIns $ putStrLn msg

echo :: (SendInsBy "tty1" m Teletype, Monad m) => m ()
echo = do
    i <- readTTY & key @"tty1"
    case i of
        "" -> pure ()
        _ -> writeTTY i & key @"tty1" >> echo

strong :: (MemberBy "tty1" Teletype ef, Forall HFunctor eh) => eh :!! ef ~> eh :!! ef
strong =
    interposeRec @("tty1" #> _) \e -> case unKey e of
        ReadTTY -> readTTY & key @"tty1"
        WriteTTY msg -> writeTTY (msg <> "!") & key @"tty1"

main :: IO ()
main = runEff $ do
    sendIns $ putStrLn "Please enter something..."
    teletypeToIO . unkeyEff @"tty1" . strong . strong $ echo
