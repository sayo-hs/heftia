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

import Control.Monad.Hefty (
    interpose,
    interpret,
    liftIO,
    makeEffectF,
    runEff,
    untag,
    type (:!!),
    type (<:),
    type (<|),
    type (~>),
 )
import Data.Effect.Tag (Tag (unTag), type (#))

data Teletype a where
    ReadTTY :: Teletype String
    WriteTTY :: String -> Teletype ()

makeEffectF [''Teletype]

teletypeToIO :: (IO <| r) => eh :!! Teletype ': r ~> eh :!! r
teletypeToIO = interpret \case
    ReadTTY -> liftIO getLine
    WriteTTY msg -> liftIO $ putStrLn msg

echo :: (Teletype # "tty1" <: m, Monad m) => m ()
echo = do
    i <- readTTY' @"tty1"
    case i of
        "" -> pure ()
        _ -> writeTTY' @"tty1" i >> echo

strong :: (Teletype # "tty1" <| ef) => eh :!! ef ~> eh :!! ef
strong =
    interpose @(_ # "tty1") \e -> case unTag e of
        ReadTTY -> readTTY' @"tty1"
        WriteTTY msg -> writeTTY' @"tty1" $ msg <> "!"

main :: IO ()
main = runEff do
    liftIO $ putStrLn "Please enter something..."
    teletypeToIO . untag @"tty1" . strong . strong $ echo
