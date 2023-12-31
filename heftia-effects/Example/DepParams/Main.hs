{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
The original of this example can be found at polysemy.
<https://hackage.haskell.org/package/polysemy>
-}
module Main where

import Control.Effect.Class (
    Taggable,
    getTag,
    sendIns,
    tag,
    type (<:),
    type (@#),
    type (~>),
 )
import Control.Effect.Class.Machinery.DepParams (type (#-))
import Control.Effect.Class.Machinery.TH (makeEffectF)
import Control.Effect.Freer (Fre, interpose, interpret, runFreerEffects, untag, type (<|-))
import Data.Free.Union (FindFirstDepParams, InsClassIn)
import Data.Function ((&))
import Data.String (IsString)
import Data.Tuple (Solo (Solo))

class Teletype s f | f -> s where
    readTTY :: f s
    writeTTY :: s -> f ()

makeEffectF ''Teletype

teletypeToIO :: (IO <: Fre es m, Monad m) => Fre (TeletypeI String ': es) m ~> Fre es m
teletypeToIO = interpret \case
    ReadTTY -> sendIns getLine
    WriteTTY msg -> sendIns $ putStrLn msg

data TTY1

echo :: (Teletype s (m @# TTY1), Monad m, Taggable m, IsString s, Eq s) => m ()
echo = do
    i <- readTTY & tag @TTY1
    case i of
        "" -> pure ()
        _ -> (writeTTY i & tag @TTY1) >> echo

strong ::
    forall s es m.
    ( I'Teletype #- TTY1 <|- es
    , Monad m
    , Semigroup s
    , IsString s
    , 'Just ('Solo s) ~ FindFirstDepParams es (I'Teletype #- TTY1)
    ) =>
    Fre es m ~> Fre es m
strong =
    interpose @(InsClassIn es (I'Teletype #- TTY1)) \e -> case getTag e of
        ReadTTY -> readTTY & tag @TTY1
        WriteTTY msg -> writeTTY (msg <> "!") & tag @TTY1

main :: IO ()
main = runFreerEffects $ do
    sendIns $ putStrLn "Please enter something..."
    teletypeToIO . untag @TTY1 . strong . strong $ echo
