{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
The original of this example can be found at polysemy.
<https://hackage.haskell.org/package/polysemy>
-}
module Main where

import Control.Effect (SendIns (sendIns), type (<:), type (~>))
import Control.Effect.ExtensibleChurch (runEff, type (:!!))
import Control.Effect.Hefty (interposeRec, interpretRec, untagEff)
import Data.Effect.TH (makeEffectF)
import Data.Effect.Tag (Tag (unTag), type (#))
import Data.Hefty.Extensible (type (<|), ForallHFunctor)

data Teletype a where
    ReadTTY :: Teletype String
    WriteTTY :: String -> Teletype ()

makeEffectF [''Teletype]

teletypeToIO :: (IO <| r, ForallHFunctor eh) => eh :!! LTeletype ': r ~> eh :!! r
teletypeToIO = interpretRec \case
    ReadTTY -> sendIns getLine
    WriteTTY msg -> sendIns $ putStrLn msg

echo :: (Teletype # "tty1" <: m, Monad m) => m ()
echo = do
    i <- readTTY' @"tty1"
    case i of
        "" -> pure ()
        _ -> writeTTY' @"tty1" i >> echo

strong :: (Teletype # "tty1" <| ef, ForallHFunctor eh) => eh :!! ef ~> eh :!! ef
strong =
    interposeRec @(_ # "tty1") \e -> case unTag e of
        ReadTTY -> readTTY' @"tty1"
        WriteTTY msg -> writeTTY' @"tty1" $ msg <> "!"

main :: IO ()
main = runEff $ do
    sendIns $ putStrLn "Please enter something..."
    teletypeToIO . untagEff @"tty1" . strong . strong $ echo
