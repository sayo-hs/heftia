{-# LANGUAGE CPP #-}

-- SPDX-License-Identifier: BSD-3-Clause
-- (c) 2022 Xy Ren; 2021-2022, Andrzej Rybcza; 2024 Sayo contributors

module Main where

import BenchCatch
import BenchCoroutine
import BenchCountdown
import BenchLocal
import BenchPyth
import Data.Functor ((<&>))
import Test.Tasty.Bench
import BenchFileSizes

main :: IO ()
main =
    defaultMain
        [ bgroup "countdown.shallow" $
            [10000] <&> \x ->
                bgroup
                    (show x)
                    [ bench "heftia" $ nf countdownHeftia x
                    -- , bench "heftia.naive" $ nf countdownHeftiaNaive x -- no optimization
#ifdef VERSION_freer_simple
                    , bench "freer" $ nf countdownFreer x
#endif
                    , bench "polyemy" $ nf countdownSem x
                    , bench "fused" $ nf countdownFused x
                    , bench "effectful" $ nf countdownEffectful x
#ifdef VERSION_eff
                    , bench "eff" $ nf countdownEff x
#endif
                    , bench "mtl" $ nf countdownMtl x
                    ]
        , bgroup "countdown.deep" $
            [10000] <&> \x ->
                bgroup
                    (show x)
                    [ bench "heftia.5+5" $ nf countdownHeftiaDeep x
                    -- , bench "heftia.naive.5+5" $ nf countdownHeftiaNaiveDeep x -- no optimization
#ifdef VERSION_freer_simple
                    , bench "freer.5+5" $ nf countdownFreerDeep x
#endif
                    , bench "polysemy.5+5" $ nf countdownSemDeep x
                    , bench "fused.5+5" $ nf countdownFusedDeep x
                    , bench "effectful.5+5" $ nf countdownEffectfulDeep x
#ifdef VERSION_eff
                    , bench "eff.5+5" $ nf countdownEffDeep x
#endif
                    , bench "mtl.5+5" $ nf countdownMtlDeep x
                    ]
        , bgroup "catch.shallow" $
            [1000] <&> \x ->
                bgroup
                    (show x)
                    [ bench "heftia" $ nf catchHeftia x
                    , bench "polysemy" $ nf catchSem x
                    , bench "fused" $ nf catchFused x
                    , bench "effectful" $ nf catchEffectful x
                    , -- , bench "eff" $ nf catchEff x
                      -- `eff` is x500 slow in this case, so it is excluded because it makes the graph hard to read.
                      bench "mtl" $ nf catchMtl x
                    ]
        , bgroup "catch.deep" $
            [10000] <&> \x ->
                bgroup
                    (show x)
                    [ bench "heftia.5+5+0" $ nf catchHeftiaDeep0 x
                    , bench "heftia.5+4+1" $ nf catchHeftiaDeep1 x
                    , bench "heftia.5+3+2" $ nf catchHeftiaDeep2 x
                    , bench "heftia.5+2+3" $ nf catchHeftiaDeep3 x
                    , bench "heftia.5+1+4" $ nf catchHeftiaDeep4 x
                    , bench "heftia.5+0+5" $ nf catchHeftiaDeep5 x
                    , bench "polysemy.5+5" $ nf catchSemDeep x
                    , bench "fused.5+5" $ nf catchFusedDeep x
                    , bench "effectful.5+5" $ nf catchEffectfulDeep x
                    , -- , bench "eff.5+5" $ nf catchEffDeep x
                      bench "mtl.5+5" $ nf catchMtlDeep x
                    ]
        , bgroup "local.shallow" $
            [10000] <&> \x ->
                bgroup
                    (show x)
                    [ bench "heftia" $ nf localHeftia x
                    , bench "polysemy" $ nf localSem x
                    , bench "fused" $ nf localFused x
                    , bench "effectful" $ nf localEffectful x
                    -- , bench "eff" $ nf localEff x
                    -- `eff` is x500 slow in this case, so it is excluded because it makes the graph hard to read.
                    --  bench "mtl" $ nf localMtl x
                    ]
        , bgroup "local.deep" $
            [10000] <&> \x ->
                bgroup
                    (show x)
                    [ bench "heftia.5+5+0" $ nf localHeftiaDeep0 x
                    , bench "heftia.5+4+1" $ nf localHeftiaDeep1 x
                    , bench "heftia.5+3+2" $ nf localHeftiaDeep2 x
                    , bench "heftia.5+2+3" $ nf localHeftiaDeep3 x
                    , bench "heftia.5+1+4" $ nf localHeftiaDeep4 x
                    , bench "heftia.5+0+5" $ nf localHeftiaDeep5 x
                    , bench "polysemy.5+5" $ nf localSemDeep x
                    , bench "fused.5+5" $ nf localFusedDeep x
                    , bench "effectful.5+5" $ nf localEffectfulDeep x
                    --  bench "eff.5+5" $ nf localEffDeep x
                    --  bench "mtl.5+5" $ nf localMtlDeep x
                    ]
        , bgroup "nondet.shallow" $
            [32] <&> \x ->
                bgroup
                    (show x)
                    [ bench "heftia" $ nf pythHeftia x
                    -- , bench "heftia.shift" $ nf pythHeftiaShift x -- tricky, slow method
#ifdef VERSION_freer_simple
                    , bench "freer" $ nf pythFreer x
#endif
                    , bench "fused" $ nf pythFused x
#ifdef VERSION_eff
                    , bench "eff" $ nf pythEff x
#endif
                    , bench "mtl-logict" $ nf pythLogict x
                    ] -- Polysemy case is excluded because of incorrect semantics.
        , bgroup "nondet.deep" $
            [32] <&> \x ->
                bgroup
                    (show x)
                    [ bench "heftia.5+5" $ nf pythHeftiaDeep x
                    , bench "heftia.shift.5+5" $ nf pythHeftiaShiftDeep x -- tricky, slow method
#ifdef VERSION_freer_simple
                    , bench "freer.5+5" $ nf pythFreerDeep x
#endif
                    , bench "fused.5+5" $ nf pythFusedDeep x
#ifdef VERSION_eff
                    , bench "eff.5+5" $ nf pythEffDeep x
#endif
                    , bench "mtl-logict.5+5" $ nf pythLogictDeep x
                    ]
        , bgroup "coroutine.shallow" $
            [1000] <&> \x ->
                bgroup
                    (show x)
                    [ bench "heftia" $ nf coroutineHeftia x
#ifdef VERSION_freer_simple
                    , bench "freer" $ nf coroutineFreer x
#endif
#ifdef VERSION_eff
                    , bench "eff" $ nf coroutineEff x
#endif
                    -- `eff` is O(n^2) slow probably because of: https://dl.acm.org/doi/10.1145/2633357.2633360
                    ] -- add mtl?
        , bgroup "coroutine.deep" $
            [1000] <&> \x ->
                bgroup
                    (show x)
                    [ bench "heftia.5+5" $ nf coroutineHeftiaDeep x
#ifdef VERSION_freer_simple
                    , bench "freer.5+5" $ nf coroutineFreerDeep x
#endif
#ifdef VERSION_eff
                    , bench "eff.5+5" $ nf coroutineEffDeep x
#endif
                    ]
        , bgroup "filesize.shallow" $ map filesizeShallow [1000, 2000, 3000]
        , bgroup "filesize.deep" $ map filesizeDeep [1000, 2000, 3000]
        ]

filesizeShallow :: Int -> Benchmark
filesizeShallow n = bgroup (show n)
  [ bench "reference" $ nfAppIO ref_calculateFileSizes (take n files)
  , bench "heftia" $ nfAppIO heftia_calculateFileSizes (take n files)
  , bench "effectful" $ nfAppIO effectful_calculateFileSizes (take n files)
#ifdef VERSION_cleff
  , bench "cleff" $ nfAppIO cleff_calculateFileSizes (take n files)
#endif
#ifdef VERSION_freer_simple
  , bench "freer-simple" $ nfAppIO fs_calculateFileSizes (take n files)
#endif
#ifdef VERSION_eff
  , bench "eff" $ nfAppIO eff_calculateFileSizes (take n files)
#endif
#ifdef VERSION_mtl
  , bench "mtl" $ nfAppIO mtl_calculateFileSizes (take n files)
#endif
#ifdef VERSION_fused_effects
  , bench "fused-effects" $ nfAppIO fe_calculateFileSizes (take n files)
#endif
#ifdef VERSION_polysemy
  , bench "polysemy" $ nfAppIO poly_calculateFileSizes (take n files)
#endif
  ]
  where
    files :: [FilePath]
    files = repeat "heftia-effects.cabal"

filesizeDeep :: Int -> Benchmark
filesizeDeep n = bgroup (show n)
  [ bench "reference" $ nfAppIO ref_calculateFileSizes (take n files)
  , bench "heftia" $ nfAppIO heftia_calculateFileSizesDeep (take n files)
  , bench "effectful" $ nfAppIO effectful_calculateFileSizesDeep (take n files)
#ifdef VERSION_cleff
  , bench "cleff" $ nfAppIO cleff_calculateFileSizesDeep (take n files)
#endif
#ifdef VERSION_freer_simple
  , bench "freer-simple" $ nfAppIO fs_calculateFileSizesDeep (take n files)
#endif
#ifdef VERSION_eff
  , bench "eff" $ nfAppIO eff_calculateFileSizesDeep (take n files)
#endif
#ifdef VERSION_mtl
  , bench "mtl" $ nfAppIO mtl_calculateFileSizesDeep (take n files)
#endif
#ifdef VERSION_fused_effects
  , bench "fused-effects" $ nfAppIO fe_calculateFileSizesDeep (take n files)
#endif
#ifdef VERSION_polysemy
  , bench "polysemy" $ nfAppIO poly_calculateFileSizesDeep (take n files)
#endif
  ]
  where
    files :: [FilePath]
    files = repeat "heftia-effects.cabal"
