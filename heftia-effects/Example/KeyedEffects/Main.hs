{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Main where

import Control.Effect (type (~>))
import Control.Effect.Key (SendFOEBy)
import Control.Monad.Hefty.Interpret (interpose, interpret, runEff)
import Control.Monad.Hefty.Transform (unkey)
import Control.Monad.Hefty.Types (type (:!!))
import Control.Monad.IO.Class (liftIO)
import Data.Effect.Key (unKey, type (#>))
import Data.Effect.OpenUnion.Internal.FO (MemberBy, type (<|))
import Data.Effect.OpenUnion.Internal.HO (HFunctors)
import Data.Effect.TH (makeEffectF)

data Teletype a where
    ReadTTY :: Teletype String
    WriteTTY :: String -> Teletype ()

makeEffectF [''Teletype]

teletypeToIO :: (IO <| r, HFunctors eh) => eh :!! Teletype ': r ~> eh :!! r
teletypeToIO = interpret \case
    ReadTTY -> liftIO getLine
    WriteTTY msg -> liftIO $ putStrLn msg

echo :: (SendFOEBy "tty1" Teletype m, Monad m) => m ()
echo = do
    i <- readTTY'' @"tty1"
    case i of
        "" -> pure ()
        _ -> writeTTY'' @"tty1" i >> echo

strong :: (MemberBy "tty1" Teletype ef, HFunctors eh) => eh :!! ef ~> eh :!! ef
strong =
    interpose @("tty1" #> _) \e -> case unKey e of
        ReadTTY -> readTTY'' @"tty1"
        WriteTTY msg -> writeTTY'' @"tty1" $ msg <> "!"

main :: IO ()
main = runEff do
    liftIO $ putStrLn "Please enter something..."
    teletypeToIO . unkey @"tty1" . strong . strong $ echo
