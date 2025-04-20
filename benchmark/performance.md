# Performance

Overall, the performance of this library is roughly on par with `effectful`, or only slightly slower.
`heftia` runs significantly faster compared to existing effect systems such as `mtl` and `polysemy`.

In the IO benchmark (which retrieves file sizes. A more down-to-earth benchmark that performs various tasks, including I/O.), it is faster compared to existing extensible effects like `mtl` and `polysemy`, and its performance is nearly on par with high-speed libraries such as `effectful` and `eff`. The State benchmark shows a similar trend.

In the Throw/Catch benchmark, it is faster than `effectful` in the fastest case, and at least 2x as fast as `polysemy` in the other cases.

In the Ask/Local benchmark, it still outperforms `polysemy` and `fused-effects` in deep effect stack cases.

Non-deterministic computations is comparable to that of other libraries.
Polysemy does not support non-deterministic computations, and among practical extensible effects libraries, only `fused-effects` currently supports them.

The coroutine performance is on par with `freer-simple`, maintaining similar speed without any noticeable degradation compared to other effects.
Moreover, the `eff`, aside from this library and `freer-simple`, likely suffer from the $O(n^2)$ issue mentioned in the "Reflection without Remorse" paper.
In comparison, `eff` are significantly slower, approximately 50x slower with $n=1000$ (as shown in the measured results) and about 500x slower with $n=10000$.

Furthermore, since there are differences among libraries in their support for higher-order effects and continuations,
please note that, for example, in the Throw/Catch benchmark, only libraries that support higher-order effects are included in the comparison.

## Reproduction
The benchmark code is available at [heftia-effects/bench](https://github.com/sayo-hs/heftia/blob/v0.6/heftia-effects/bench).
To run the benchmarks, move your working directory to the root directory of the `heftia` repository and execute
 [`./benchmark/bench.sh`](https://github.com/sayo-hs/heftia/blob/v0.6/benchmark/bench.sh).

## Benchmark Results

The code was compiled with GHC 9.8.4 and run on a Ryzen 9 3900XT.

* IO Benchmark (Shallow Effect Stack):
![filesize.shallow](https://github.com/sayo-hs/heftia/blob/v0.6/benchmark/bench-result/filesize-shallow.svg)

* IO Benchmark (Deep Effect Stack):
![filesize.deep](https://github.com/sayo-hs/heftia/blob/v0.6/benchmark/bench-result/filesize-deep.svg)

* State Effect Benchmark (Shallow Effect Stack):
![countdown.shallow](https://github.com/sayo-hs/heftia/blob/v0.6/benchmark/bench-result/countdown-shallow.svg)

* State Effect Benchmark (Deep Effect Stack):
![countdown.deep](https://github.com/sayo-hs/heftia/blob/v0.6/benchmark/bench-result/countdown-deep.svg)

* Throw/Catch Effect Benchmark (Shallow Effect Stack):
![catch.shallow](https://github.com/sayo-hs/heftia/blob/v0.6/benchmark/bench-result/catch-shallow.svg)

* Throw/Catch Effect Benchmark (Deep Effect Stack):
![catch.deep](https://github.com/sayo-hs/heftia/blob/v0.6/benchmark/bench-result/catch-deep.svg)

Note: Here, the $5 + (5 - N) + N$ corresponds to the interpretation stack of `catchHeftiaDeepN` represented below:

```haskell
catchHeftiaDeep0, catchHeftiaDeep1, catchHeftiaDeep2, catchHeftiaDeep3, catchHeftiaDeep4, catchHeftiaDeep5 :: Int -> Either () ()
catchHeftiaDeep0 n = Heftia.runPure $ run $ run $ run $ run $ run $ Heftia.runThrow $ run $ run $ run $ run $ run $ Heftia.runCatch @() $ programHeftia n
catchHeftiaDeep1 n = Heftia.runPure $ run $ run $ run $ run $ run $ Heftia.runThrow $ run $ run $ run $ run $ Heftia.runCatch @() $ run $ programHeftia n
catchHeftiaDeep2 n = Heftia.runPure $ run $ run $ run $ run $ run $ Heftia.runThrow $ run $ run $ run $ Heftia.runCatch @() $ run $ run $ programHeftia n
catchHeftiaDeep3 n = Heftia.runPure $ run $ run $ run $ run $ run $ Heftia.runThrow $ run $ run $ Heftia.runCatch @() $ run $ run $ run $ programHeftia n
catchHeftiaDeep4 n = Heftia.runPure $ run $ run $ run $ run $ run $ Heftia.runThrow $ run $ Heftia.runCatch @() $ run $ run $ run $ run $ programHeftia n
catchHeftiaDeep5 n = Heftia.runPure $ run $ run $ run $ run $ run $ Heftia.runThrow $ Heftia.runCatch @() $ run $ run $ run $ run $ run $ programHeftia n

run :: Heftia.Eff eh (Heftia.Ask () ': ef) a -> Heftia.Eff eh ef a
run = Heftia.runAsk ()
```

* Ask/Local Effect Benchmark (Shallow Effect Stack):
![local.shallow](https://github.com/sayo-hs/heftia/blob/v0.6/benchmark/bench-result/local-shallow.svg)

* Ask/Local Effect Benchmark (Deep Effect Stack):
![local.deep](https://github.com/sayo-hs/heftia/blob/v0.6/benchmark/bench-result/local-deep.svg)

* NonDet Effect Benchmark (Shallow Effect Stack):
![nondet.shallow](https://github.com/sayo-hs/heftia/blob/v0.6/benchmark/bench-result/nondet-shallow.svg)

* NonDet Effect Benchmark (Deep Effect Stack):
![nondet.deep](https://github.com/sayo-hs/heftia/blob/v0.6/benchmark/bench-result/nondet-deep.svg)

* Coroutine Benchmark (Shallow Effect Stack):
![coroutine.shallow](https://github.com/sayo-hs/heftia/blob/v0.6/benchmark/bench-result/coroutine-shallow.svg)

* Coroutine Benchmark (Deep Effect Stack):
![coroutine.deep](https://github.com/sayo-hs/heftia/blob/v0.6/benchmark/bench-result/coroutine-deep.svg)
