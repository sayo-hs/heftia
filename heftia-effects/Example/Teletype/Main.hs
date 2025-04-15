{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}

-- SPDX-License-Identifier: MPL-2.0

{- |
The original of this example can be found at polysemy.
<https://hackage.haskell.org/package/polysemy>
-}
module Main where

import Control.Monad.Hefty (
    Eff,
    Effect,
    Emb,
    interpose,
    interpret,
    liftIO,
    makeEffectF,
    runEff,
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

echo :: (Teletype :> es) => Eff es ()
echo = do
    i <- readTTY
    case i of
        "" -> pure ()
        _ -> writeTTY i >> echo

strong :: (Teletype :> es) => Eff es ~> Eff es
strong =
    interpose \case
        ReadTTY -> readTTY
        WriteTTY msg -> writeTTY $ msg <> "!"

main :: IO ()
main = runEff do
    liftIO $ putStrLn "Please enter something..."
    teletypeToIO . strong . strong $ echo
