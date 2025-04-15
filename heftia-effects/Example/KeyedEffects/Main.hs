{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Main where

import Control.Monad.Hefty (
    Eff,
    Effect,
    Emb,
    Has,
    interposeOn,
    interpret,
    liftIO,
    makeEffectF,
    runEff,
    untag,
    (:>),
    type (~>),
 )

data Teletype :: Effect where
    ReadTTY :: Teletype f String
    WriteTTY :: String -> Teletype f ()

makeEffectF ''Teletype

teletypeToIO :: (Emb IO :> es) => Eff (Teletype ': es) ~> Eff es
teletypeToIO = interpret \case
    ReadTTY -> liftIO getLine
    WriteTTY msg -> liftIO $ putStrLn msg

echo :: (Has "tty1" Teletype es) => Eff es ()
echo = do
    i <- readTTY' @"tty1"
    case i of
        "" -> pure ()
        _ -> writeTTY' @"tty1" i >> echo

strong :: (Has "tty1" Teletype es) => Eff es ~> Eff es
strong =
    interposeOn @"tty1" \case
        ReadTTY -> readTTY' @"tty1"
        WriteTTY msg -> writeTTY' @"tty1" $ msg <> "!"

main :: IO ()
main = runEff do
    liftIO $ putStrLn "Please enter something..."
    teletypeToIO . untag @"tty1" . strong . strong $ echo
