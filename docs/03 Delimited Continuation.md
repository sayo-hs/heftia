# Example 3 - Delimited Continuation

In this chapter, using an example, we'll demonstrate how `heftia-effects` facilitates the handling of delimited continuations.

## `fork` effect

The `fork` effect defined below non-deterministically branches control structures.

Speaking of non-deterministic computations, there's the list monad. Just like how in the list monad one does `do { x <- [1..4]; ... }`, this effect branches the flow of control, creating parallel worlds.

Readers familiar with Posix programming can think of this as the so-called `fork()` function that splits processes. However, while the Posix `fork` function bifurcates the control structure into two, this one can bifurcate into any number.

```hs
type ForkID = Int

class Fork f where
    fork :: f ForkID

makeEffectF ''Fork

runForkSingle :: Monad m => Fre (ForkI ': r) m ~> Fre r m
runForkSingle = interpret \Fork -> pure 0
```

The returned `ForkID` is an ID indicating which branch of the world one is in after the split. The `runForkSingle` is a simple interpreter that returns only `0` without branching even if a `fork` effect is thrown.

## `delimitFork` effect

The `delimitFork` effect below is a higher-order effect used to delimit the range of branching by scope.

```hs
class DelimitFork f where
    delimitFork :: Monoid w => f w -> f w

makeEffectH ''DelimitFork
```

The difference from the Posix `fork()` is that while the former indefinitely maintains its branches until the process ends, the latter has a limited branching range.

When exiting the scope introduced by `delimitFork`, the branches caused by `fork` within the scope all converge, and the return value is composed according to the `Monoid`.

Now, it's the turn for delimited continuations. I won't explain here what a delimited continuation is, as there should be a better explanation elsewhere. Just to mention that "delimited" means that the branching remains within the scope, and outside that scope, the branching does not continue.

The elaborator below branches the control structure into `numberOfFork` units at the time the `fork` effect is thrown by extracting the delimited continuation corresponding to the scope.

```hs
applyDelimitFork :: (ForkI <| es, Monad m) => Int -> Elaborator DelimitForkS (Fre es m)
applyDelimitFork numberOfFork (DelimitFork m) =
    m & interposeK pure \k Fork -> do
        r <- mapM k [1 .. numberOfFork]
        pure $ mconcat r
```

Here, `Elaborator` is simply a type synonym defined as `type Elaborator e f = e f ~> f`.

To extract delimited continuations, one can use functions like `interposeK`. There are several other `K`-related functions, so choose the one that fits the application. Here, the extracted delimited continuation `k` is called from `1` to `numberOfFork`, and finally, the results of each continuation are aggregated with `mconcat`.

Let's look at an example of execution.

```hs
main :: IO ()
main =
    runFreerEffects
        . runForkSingle
        . runElaborate @_ @HeftiaChurchT @SumUnionH (applyDelimitFork 4 |+: absurdUnionH)
        $ do
            sendIns . putStrLn . (("[out of scope] " ++) . show) =<< fork
            s <- delimitFork do
                fid1 <- fork
                fid2 <- fork
                sendIns $ putStrLn $ "[delimited continuation of `fork`] Fork ID: " ++ show (fid1, fid2)
                pure $ show (fid1, fid2)
            sendIns $ putStrLn $ "scope exited. result: " ++ s

{- Execution result:
[out of scope] 0
[delimited continuation of `fork`] Fork ID: (1,1)
[delimited continuation of `fork`] Fork ID: (1,2)
[delimited continuation of `fork`] Fork ID: (1,3)
[delimited continuation of `fork`] Fork ID: (1,4)
[delimited continuation of `fork`] Fork ID: (2,1)
[delimited continuation of `fork`] Fork ID: (2,2)
[delimited continuation of `fork`] Fork ID: (2,3)
[delimited continuation of `fork`] Fork ID: (2,4)
[delimited continuation of `fork`] Fork ID: (3,1)
[delimited continuation of `fork`] Fork ID: (3,2)
[delimited continuation of `fork`] Fork ID: (3,3)
[delimited continuation of `fork`] Fork ID: (3,4)
[delimited continuation of `fork`] Fork ID: (4,1)
[delimited continuation of `fork`] Fork ID: (4,2)
[delimited continuation of `fork`] Fork ID: (4,3)
[delimited continuation of `fork`] Fork ID: (4,4)
scope exited. result: (1,1)(1,2)(1,3)(1,4)(2,1)(2,2)(2,3)(2,4)(3,1)(3,2)(3,3)(3,4)(4,1)(4,2)(4,3)(4,4)
-}
```

First, outside the `delimitFork` scope, it's unaffected by `applyDelimitFork`, and only reflects the behavior of `runForkSingle`. Within the `delimitFork` scope, like in a list monad, for each `fork`, all patterns from `1` to `4` are returned, and the computation branches forward. When exiting the `delimitFork` scope, the branches converge, and the returned strings in the format `(fid1, fid2)` are all concatenated as a result using the `Monoid`.

---

Thus, with `heftia-effects`, it's easy to handle delimited continuations. Moreover, whether or how to use (or not use) delimited continuations can be flexibly (and modularly) altered through elaboration. This will probably enable the creation of various interesting effects, such as non-deterministic computations and a modifiable Async effect for concurrent computations with a changeable asynchronous backend.

There are many libraries that implement the Effect System in Haskell, but as far as the author knows, the only one that can almost fully emulate what's possible with the renowned "Algebraic Effects and Handlers" (e.g., extracting delimited continuations and using them for modular effect handling) is based on this approach rooted in Hefty Algebras.

## Complete code

```hs
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Effect.Class (sendIns, type (~>))
import Control.Effect.Class.Machinery.TH (makeEffectF, makeEffectH)
import Control.Effect.Freer (Fre, interposeK, interpret, runFreerEffects, type (<|))
import Control.Effect.Heftia (Elaborator, runElaborate)
import Control.Monad.Trans.Heftia.Church (HeftiaChurchT)
import Data.Function ((&))
import Data.Hefty.Sum (SumUnionH)
import Data.Hefty.Union (UnionH (absurdUnionH, (|+:)))

type ForkID = Int

class Fork f where
    fork :: f ForkID

makeEffectF ''Fork

runForkSingle :: Monad m => Fre (ForkI ': r) m ~> Fre r m
runForkSingle = interpret \Fork -> pure 0

class DelimitFork f where
    delimitFork :: Monoid w => f w -> f w

makeEffectH ''DelimitFork

applyDelimitFork :: (ForkI <| es, Monad m) => Int -> Elaborator DelimitForkS (Fre es m)
applyDelimitFork numberOfFork (DelimitFork m) =
    m & interposeK pure \k Fork -> do
        r <- mapM k [1 .. numberOfFork]
        pure $ mconcat r

main :: IO ()
main =
    runFreerEffects
        . runForkSingle
        . runElaborate @_ @HeftiaChurchT @SumUnionH (applyDelimitFork 4 |+: absurdUnionH)
        $ do
            sendIns . putStrLn . (("[out of scope] " ++) . show) =<< fork
            s <- delimitFork do
                fid1 <- fork
                fid2 <- fork
                sendIns $ putStrLn $ "[delimited continuation of `fork`] Fork ID: " ++ show (fid1, fid2)
                pure $ show (fid1, fid2)
            sendIns $ putStrLn $ "scope exited. result: " ++ s
```
