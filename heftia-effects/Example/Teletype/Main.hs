{-# LANGUAGE TemplateHaskell #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Main where

import Control.Effect.Class (type (~>))
import Control.Effect.Class.Embed (Embed, EmbedI (Embed), embed)
import Control.Effect.Class.Machinery.TH (makeEffectF)
import Control.Effect.Freer (Fre, freerEffects, interpret, interpreted)
import Control.Freer.Trans (liftLower)
import Control.Monad.IO.Class (MonadIO (liftIO))

class Teletype f where
    readTTY :: f String
    writeTTY :: String -> f ()

makeEffectF ''Teletype

teletypeToIO :: (Embed IO (Fre es m), Monad m) => Fre (TeletypeI ': es) m ~> Fre es m
teletypeToIO = interpret \case
    ReadTTY -> embed getLine
    WriteTTY msg -> embed $ putStrLn msg

runEmbedIO :: MonadIO m => Fre (EmbedI IO ': es) m ~> Fre es m
runEmbedIO = interpret \(Embed m) -> freerEffects $ liftLower $ liftIO m

echo :: (Teletype m, Monad m) => m ()
echo = do
    i <- readTTY
    case i of
        "" -> pure ()
        _ -> writeTTY i >> echo

main :: IO ()
main = interpreted . runEmbedIO $ do
    embed $ putStrLn "Please enter something..."
    teletypeToIO echo
