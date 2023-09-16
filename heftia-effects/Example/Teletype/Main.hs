{-# LANGUAGE TemplateHaskell #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
The original of this example can be found at polysemy.
<https://hackage.haskell.org/package/polysemy>
-}
module Main where

import Control.Effect.Class (Taggable, getTag, sendIns, tag, type (#), type (<:), type (@#), type (~>))
import Control.Effect.Class.Machinery.TH (makeEffectF)
import Control.Effect.Freer (Fre, interpose, interpret, runFreerEffects, untag, type (<|))
import Data.Function ((&))

class Teletype f where
    readTTY :: f String
    writeTTY :: String -> f ()

makeEffectF ''Teletype

teletypeToIO :: (IO <: Fre es m, Monad m) => Fre (TeletypeI ': es) m ~> Fre es m
teletypeToIO = interpret \case
    ReadTTY -> sendIns getLine
    WriteTTY msg -> sendIns $ putStrLn msg

data TTY1

echo :: (Teletype (m @# TTY1), Monad m, Taggable m) => m ()
echo = do
    i <- readTTY & tag @TTY1
    case i of
        "" -> pure ()
        _ -> (writeTTY i & tag @TTY1) >> echo

strong :: (TeletypeI # TTY1 <| es, Monad m) => Fre es m ~> Fre es m
strong =
    interpose @(_ # TTY1) \e -> case getTag e of
        ReadTTY -> readTTY & tag @TTY1
        WriteTTY msg -> writeTTY (msg <> "!") & tag @TTY1

main :: IO ()
main = runFreerEffects $ do
    sendIns $ putStrLn "Please enter something..."
    teletypeToIO . untag @TTY1 . strong . strong $ echo
