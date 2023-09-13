{-# LANGUAGE TemplateHaskell #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Main where

import Control.Effect.Class (type (~>))
import Control.Effect.Class.Embed (embed)
import Control.Effect.Class.Machinery.TH (makeEffectF, makeEffectH)
import Control.Effect.Freer (Fre, interposeK, interpret, type (<:))
import Control.Effect.Handler.Heftia.Embed (runEmbed)
import Control.Effect.Heftia (Elaborator, runElaborate)
import Control.Monad.Trans.Heftia.Church (HeftiaChurchT)
import Data.Function ((&))
import Data.Hefty.Sum (SumUnionH)
import Data.Hefty.Union (UnionH (absurdUnionH, (|+:)))

type ForkID = Int

class Fork f where
    fork :: f ForkID
makeEffectF ''Fork

runFork :: Monad m => Fre (ForkI ': r) m ~> Fre r m
runFork = interpret \Fork -> pure 0

class DelimitFork f where
    delimitFork :: Monoid w => f w -> f w
makeEffectH ''DelimitFork

applyDelimitFork ::
    (ForkI <: es, Monad m) =>
    Int ->
    Elaborator DelimitForkS (Fre es m)
applyDelimitFork numberOfFork (DelimitFork m) =
    m & interposeK pure \k Fork -> do
        r <- mapM k [1 .. numberOfFork]
        pure $ mconcat r

{-
-- In the `mconcat` section, we utilize the fact that `w` in `delimitFork` is a `Monoid`.
-- However, `hoistHeftiaEffects` quantifies `w` into any type, so we can't make use of
-- it being a `Monoid`. Thus, writing it this way results in a type error.

runDelimitFork ::
    (ForkI <: es, HFunctor (SumH r), Monad m) =>
    Int ->
    Hef (DelimitForkS ': r) (Fre es m) ~> Hef r (Fre es m)
runDelimitFork numberOfFork =
    interpretH \(DelimitFork m) ->
        ($ m) $ hoistHeftiaEffects $ interposeK pure \k Fork -> do
            r <- mapM k [1 .. numberOfFork]
            pure $ mconcat r -- Here's where the type error occurs
-}

main :: IO ()
main =
    runEmbed
        . runFork
        . runElaborate @_ @HeftiaChurchT @SumUnionH (applyDelimitFork 4 |+: absurdUnionH)
        $ do
            embed . putStrLn . (("[out of scope] " ++) . show) =<< fork
            s <- delimitFork do
                wid1 <- fork
                wid2 <- fork
                embed $ putStrLn $ "[delimited continuation of `fork`] Fork ID: " ++ show (wid1, wid2)
                pure $ show (wid1, wid2)
            embed $ putStrLn $ "scope exited. result: " ++ s
