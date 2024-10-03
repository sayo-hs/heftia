-- SPDX-License-Identifier: BSD-3-Clause
-- (c) 2022 Xy Ren; 2024 Sayo Koyoneda

module Main where

import BenchCatch
import BenchCoroutine
import BenchCountdown
import BenchPyth
import Data.Functor ((<&>))
import Test.Tasty.Bench

main :: IO ()
main =
    defaultMain
        [ bgroup "countdown" $
            [10000] <&> \x ->
                bgroup
                    (show x)
                    [ bench "heftia.shallow" $ nf countdownHeftia x
                    , bench "heftia.deep" $ nf countdownHeftiaDeep x
                    , bench "freer.shallow" $ nf countdownFreer x
                    , bench "freer.deep" $ nf countdownFreerDeep x
                    , bench "polyemy.shallow" $ nf countdownSem x
                    , bench "polysemy.deep" $ nf countdownSemDeep x
                    , bench "fused.shallow" $ nf countdownFused x
                    , bench "fused.deep" $ nf countdownFusedDeep x
                    , bench "effectful.shallow" $ nf countdownEffectful x
                    , bench "effectful.deep" $ nf countdownEffectfulDeep x
                    , bench "eff.shallow" $ nf countdownEff x
                    , bench "eff.deep" $ nf countdownEffDeep x
                    , bench "ev.shallow" $ nf countdownEv x
                    , bench "ev.deep" $ nf countdownEvDeep x
                    , bench "mtl.shallow" $ nf countdownMtl x
                    , bench "mtl.deep" $ nf countdownMtlDeep x
                    ]
        , bgroup "catch" $
            [10000] <&> \x ->
                bgroup
                    (show x)
                    [ bench "heftia.shallow" $ nf catchHeftia x
                    , bench "heftia.deep0" $ nf catchHeftiaDeep0 x
                    , bench "heftia.deep1" $ nf catchHeftiaDeep1 x
                    , bench "heftia.deep2" $ nf catchHeftiaDeep2 x
                    , bench "heftia.deep3" $ nf catchHeftiaDeep3 x
                    , bench "heftia.deep4" $ nf catchHeftiaDeep4 x
                    , bench "heftia.deep5" $ nf catchHeftiaDeep5 x
                    , bench "polysemy.shallow" $ nf catchSem x
                    , bench "polysemy.deep" $ nf catchSemDeep x
                    , bench "fused.shallow" $ nf catchFused x
                    , bench "fused.deep" $ nf catchFusedDeep x
                    , bench "effectful.shallow" $ nf catchEffectful x
                    , bench "effectful.deep" $ nf catchEffectfulDeep x
                    , -- , bench "eff.shallow" $ nf catchEff x
                      -- , bench "eff.deep" $ nf catchEffDeep x
                      -- `eff` is x500 slow in this case, so it is excluded because it makes the graph hard to read.
                      bench "mtl.shallow" $ nf catchMtl x
                    , bench "mtl.deep" $ nf catchMtlDeep x
                    ]
        , bgroup "nondet" $
            [32] <&> \x ->
                bgroup
                    (show x)
                    [ bench "heftia.shallow" $ nf pythHeftia x
                    , bench "heftia.deep" $ nf pythHeftiaDeep x
                    , bench "freer.shallow" $ nf pythFreer x
                    , bench "freer.deep" $ nf pythFreerDeep x
                    , bench "fused.shallow" $ nf pythFused x
                    , bench "fused.deep" $ nf pythFusedDeep x
                    , bench "ev.shallow" $ nf pythEv x
                    , bench "ev.deep" $ nf pythEvDeep x
                    , bench "mp.shallow" $ nf pythMp x
                    , bench "mp.deep" $ nf pythMpDeep x
                    , bench "eff.shallow" $ nf pythEff x
                    , bench "eff.deep" $ nf pythEffDeep x
                    , bench "mtl-logict.shallow" $ nf pythLogict x
                    , bench "mtl-logict.deep" $ nf pythLogictDeep x
                    ] -- Polysemy case is excluded because of incorrect semantics.
        , bgroup "coroutine" $
            [1000] <&> \x ->
                bgroup
                    (show x)
                    [ bench "heftia.shallow" $ nf coroutineHeftia x
                    , bench "heftia.deep" $ nf coroutineHeftiaDeep x
                    , bench "freer.shallow" $ nf coroutineFreer x
                    , bench "freer.deep" $ nf coroutineFreerDeep x
                    , bench "eff.shallow" $ nf coroutineEff x
                    , bench "eff.deep" $ nf coroutineEffDeep x
                    , bench "mp.shallow" $ nf coroutineMp x
                    , bench "mp.deep" $ nf coroutineMpDeep x
                    -- `mpeff` is O(n^2) slow because of: https://dl.acm.org/doi/10.1145/2633357.2633360
                    -- `eff` is probably for the same reason.
                    ]
                    -- add mtl?
        ]
