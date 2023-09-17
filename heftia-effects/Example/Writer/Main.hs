-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Main where

import Control.Effect.Class (sendIns, type (~>))
import Control.Effect.Class.Machinery.HFunctor (HFunctor)
import Control.Effect.Class.Writer (Writer, censor, tell)
import Control.Effect.Freer (runFreerEffects)
import Control.Effect.Handler.Heftia.Writer (
    elaborateWriterT,
    elaborateWriterTransactionalT,
    interpretTell,
 )
import Control.Effect.Heftia (Elaborator, Hef, runElaborate)
import Data.Hefty.Union (absurdUnionH, (|+:))

hello :: (Writer String m, Monad m) => m ()
hello = do
    tell "Hello"
    tell " world!"

censorHello :: (Writer String m, Monad m) => m ()
censorHello =
    censor
        (\s -> if s == "Hello" then "Goodbye" else s)
        hello

main :: IO ()
main = runFreerEffects do
    (s :: String, _) <-
        interpretTell
            . runElaborate' (elaborateWriterT @String)
            $ censorHello

    (sTransactional :: String, _) <-
        interpretTell
            . runElaborate' (elaborateWriterTransactionalT @String)
            $ censorHello

    sendIns $ putStrLn $ "Normal: " <> s
    sendIns $ putStrLn $ "Transactional: " <> sTransactional

runElaborate' ::
    (HFunctor e, Monad f) =>
    Elaborator e f ->
    Hef '[e] f ~> f
runElaborate' f = runElaborate $ f |+: absurdUnionH
