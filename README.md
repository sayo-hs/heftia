# Heftia: A theory‑backed, ultra type‑safe algebraic effects

[![Hackage](https://img.shields.io/hackage/v/heftia.svg?logo=haskell&label=heftia)](https://hackage.haskell.org/package/heftia)
[![Hackage](https://img.shields.io/hackage/v/heftia-effects.svg?logo=haskell&label=heftia-effects)](https://hackage.haskell.org/package/heftia-effects)
[![Stackage LTS](https://www.stackage.org/package/heftia-effects/badge/lts)](https://www.stackage.org/lts/package/heftia-effects)
[![Stackage Nightly](https://www.stackage.org/package/heftia-effects/badge/nightly)](https://www.stackage.org/nightly/package/heftia-effects)
[![Build status](https://img.shields.io/github/actions/workflow/status/sayo-hs/heftia/haskell.yml?branch=develop)](https://github.com/sayo-hs/heftia/actions)

Heftia is a Haskell library for algebraic effects grounded in solid theoretical foundations.

It is the only library that fully supports higher-order algebraic effects with complete type safety.

Say goodbye to runtime errors and unsound semantics.

This library provides a simple interface, predictable behavior, and maximum flexibility while delivering standard, practical performance.

Please refer to the [Haddock documentation](https://hackage.haskell.org/package/heftia-0.6.0.0/docs/Control-Monad-Hefty.html) for usage and semantics.
For information on performance, please refer to [performance.md](https://github.com/sayo-hs/heftia/blob/v0.6.0/benchmark/performance.md).

This library is inspired by the paper:
* [Hefty Algebras: Modular Elaboration of Higher-Order Algebraic Effects. Casper Bach Poulsen & Cas van der Rest, POPL 2023.](https://doi.org/10.1145/3571255)

The elaboration approach proposed in that paper allows for a straightforward treatment of higher-order effects.

Heftia is also grounded in the following theory:
* [A Framework for Higher-Order Effects & Handlers. Birthe van den Berg & Tom Schrijvers, Sci. Comput. Program. 2024.](https://doi.org/10.1016/j.scico.2024.103086)

## Why choose this library over others?
This library is based on algebraic effects. Currently, **none of the practical effect libraries other than this one are "algebraic."** So, why is being *algebraic* important?

For example, algebraic effects are essential for managing coroutines, generators, streaming, concurrency, non-deterministic computations, and more in a highly elegant and concise manner.

Algebraic effects provide a consistent and predictable framework for handling side effects.
Research in cutting-edge languages like [Koka](https://koka-lang.github.io/koka/doc/index.html), [Eff lang](https://www.eff-lang.org/), and [OCaml 5](https://ocaml.org/manual/effects.html) is advancing the understanding and implementation of algebraic effects, establishing them as **the programming paradigm of the future**.

Heftia extends this by supporting higher-order algebraic effects, allowing for more expressive and modular effect management.
This positions Heftia at **the forefront of modern effect handling techniques**.

Furthermore, **Heftia is functionally a superset of other effect libraries**, especially those based on `ReaderT` over `IO`.
In other words, anything that is possible with other libraries is also possible with this library.
This is because Heftia supports `MonadUnliftIO` in the form of higher-order effects.

`MonadUnliftIO` is a typeclass that ensures safety in exception handling.
Heftia completely resolves runtime-error issues present in certain usages of `MonadUnliftIO` with `effectful` and `bluefin`, thereby demonstrating **stronger safety guarantees**[^8][^9].
Moreover, Heftia delivers **strong performance**.

**Heftia should be a good substitute for `mtl`, `effectful`, `polysemy`, `fused-effects`, and `freer-simple`.**
If performance is particularly important, [`effectful`](https://github.com/haskell-effectful/effectful) would be the best alternative to this library.

[^8]: MonadUnliftIO instance allows escape https://github.com/tomjaguarpaw/bluefin/issues/29
[^9]: [heftia-effects/test/Test/UnliftIO.hs](https://github.com/sayo-hs/heftia/blob/v0.6.0/heftia-effects/test/Test/UnliftIO.hs)

## Key Features

* **Correct Semantics for Higher-Order Effects & Continuations**

    This library provides the following features simultaneously, which existing libraries could not support together:

    * Delimited continuations (algebraic effects)
        * Coroutines (non-scoped resumptions)
        * Coroutine-based, composable, and resumable concurrent streams
        * Non-deterministic computations

    * Higher-order effects
        * [`MonadUnliftIO`](https://hackage.haskell.org/package/unliftio)
            * to prevent resource leaks due to runtime exceptions
            * [heftia-effects/Example/UnliftIO/Main.hs](https://github.com/sayo-hs/heftia/blob/v0.6.0/heftia-effects/Example/UnliftIO/Main.hs)
            * [heftia-effects/Example/Stream/Main.hs](https://github.com/sayo-hs/heftia/blob/v0.6.0/heftia-effects/Example/Stream/Main.hs)
        * [`Provider`](https://hackage.haskell.org/package/effectful-core-2.5.0.0/docs/Effectful-Provider.html) a.k.a. [`Scoped`](https://hackage.haskell.org/package/polysemy-1.9.2.0/docs/Polysemy-Scoped.html)
            * to prevent [resource handles from leaking out of scopes](https://h2.jaguarpaw.co.uk/posts/bluefin-prevents-handles-leaking/)
            * [Control.Monad.Hefty.Concurrent.Subprocess](https://hackage.haskell.org/package/heftia-effects-0.6.0.0/docs/Control-Monad-Hefty-Concurrent-Subprocess.html)
            * [heftia-effects/Example/Subprocess/Main.hs](https://github.com/sayo-hs/heftia/blob/v0.6.0/heftia-effects/Example/Subprocess/Main.hs)
            * [heftia-effects/Example/DatabaseProvider/Main.hs](https://github.com/sayo-hs/heftia/blob/v0.6.0.1/heftia-effects/Example/DatabaseProvider/Main.hs)
        * [Applicative-style Parallelism](https://medium.com/@PerrottaFrancisco/learning-cats-effects-parallel-execution-f617f883e390)
            * like `cats-effect` in Scala
            * [Data.Effect.Concurrent.Parallel](https://hackage.haskell.org/package/data-effects-0.3.0.1/docs/Data-Effect-Concurrent-Parallel.html)
            * [Control.Monad.Hefty.Concurrent.Parallel](https://hackage.haskell.org/package/heftia-effects-0.6.0.0/docs/Control-Monad-Hefty-Concurrent-Parallel.html)
            * [heftia-effects/test/Test/Concurrent.hs](https://github.com/sayo-hs/heftia/blob/v0.6.0/heftia-effects/test/Test/Concurrent.hs)

    All of these interact through a simple, consistent, and predictable semantics based on algebraic effects.

* **Easy and Concise Implementation for Custom Effect Interpreters**

    As you can see from the implementations of basic effect interpreters such as [State](https://hackage.haskell.org/package/heftia-effects-0.6.0.0/docs/src/Control.Monad.Hefty.State.html#runState), [Throw/Catch](https://hackage.haskell.org/package/heftia-effects-0.6.0.0/docs/src/Control.Monad.Hefty.Except.html#runThrow), [Writer](https://hackage.haskell.org/package/heftia-effects-0.6.0.0/docs/src/Control.Monad.Hefty.Writer.html#runTell), [NonDet](https://hackage.haskell.org/package/heftia-effects-0.6.0.0/docs/src/Control.Monad.Hefty.NonDet.html#runNonDet), and [Coroutine](https://hackage.haskell.org/package/heftia-effects-0.6.0.0/docs/src/Control.Monad.Hefty.Coroutine.html#runCoroutine), they can be implemented in just a few lines, or even a single line. Even for effects like NonDet and Coroutine, which involve continuations and might seem difficult to implement at first glance, this is exactly how simple it can be. This is the power of algebraic effects. Users can quickly define experimental and innovative custom effects using continuations.

* **Standard and Reasonable Performance**

    It operates at a speed positioned roughly in the middle between faster libraries (like `effectful` or `eveff`) and relatively slower ones (like `polysemy` or `fused-effects`): [performance.md](https://github.com/sayo-hs/heftia/blob/v0.6.0/benchmark/performance.md).

* **Type Safety and Purity**

    * Does not depend on the IO monad and can use any monad as the base monad.
    * Semantics are isolated from the IO monad, meaning that aspects like asynchronous exceptions and threads do not affect the behavior of effects.
    * The constructors of the `Eff` monad are [exposed](https://hackage.haskell.org/package/heftia-0.6.0.0/docs/Control-Monad-Hefty.html#t:Eff), and users can manipulate them directly without any safety concerns. Still, the semantics remain intact.
    * These are in contrast to libraries like `effectful` and `eff`, making this library more **Haskell-ish and purely functional**.
    * **This design effectively prevents obscure behaviors and potential runtime errors.**

* **Approach to Inter-Library Compatibility**

    * Built on the [`data-effects`](https://github.com/sayo-hs/data-effects) effect framework, `heftia` is designed so that it can integrate smoothly with other effect libraries that are built upon the same framework.
    * Conversion between different libraries' `Eff` monads.
    * `interpret` functions that works independently of any particular library.
    * At present, only `heftia` is based on this framework.
    * This represents an initial attempt to resolve the issues of incompatibility and lack of interoperability caused by the proliferation of effect libraries in Haskell.
    * In addition to monads, an effect system built on Applicative and Functor is also available.

## Downsides

This library has notable semantic differences, particularly compared to libraries like `effectful`, `polysemy`, and `fused-effects`.
The semantics of this library are almost equivalent to those of `freer-simple` and are also similar to Alexis King's `eff` library.
This type of semantics is often referred to as *continuation-based semantics*.
Additionally, unlike recent libraries such as `effectful`, which have an IO-fused effect system, the semantics of this library are separated from IO.
People who are already familiar with the behaviors of other major libraries might potentially find it somewhat challenging to transition to this library, due to differences in their mental models.

For those who have not used an extensible effects library in Haskell before, this should not be a problem.
Particularly, if you are already somewhat familiar with the semantics of algebraic effects through languages like `koka` or `eff-lang`,
you likely already have the mental model needed for this library, and everything should go smoothly.

## Status

This library is currently in the beta stage.
There may be significant changes and potential bugs.

**I am looking forward to your feedback!**

## Getting Started
1.
    ```console
    $ cabal update
    ```
2. Add `heftia-effects ^>= 0.6` to the build dependencies. Enable the `GHC2021` and the following language extensions as needed:

    * `LambdaCase`
    * `DerivingStrategies`
    * `DataKinds`
    * `TypeFamilies`
    * `BlockArguments`
    * `FunctionalDependencies`
    * `RecordWildCards`
    * `DefaultSignatures`
    * `PatternSynonyms`

Example .cabal:

```
...
    build-depends:
        ...
        heftia-effects ^>= 0.6,

    default-language: GHC2021

    default-extensions:
        ...
        LambdaCase,
        DerivingStrategies,
        DataKinds,
        TypeFamilies,
        BlockArguments,
        FunctionalDependencies,
        RecordWildCards,
        DefaultSignatures,
        PatternSynonyms,
        TemplateHaskell,
        PartialTypeSignatures,
        AllowAmbiguousTypes
...
```

If you encounter an error like the following, add the pragma:

```haskell
{-# OPTIONS_GHC -fconstraint-solver-iterations=16 #-}
```

to the header of your source file.

    solveWanteds: too many iterations (limit = 4)

Here, the number 16 should be set to the maximum number of effects you want to stack.
The default in GHC is 4, which is quite low, so it's a good idea to set it to around 16 globally, rather than specifying the pragma in each file individually.

The supported versions are GHC 9.6.2 and later.

## Example

### Coroutine-based Composable Concurrent Stream

Below is an example of using concurrent streams (pipes).

```haskell
{-# OPTIONS_GHC -fconstraint-solver-iterations=16 #-}

import Control.Monad.Hefty
import Control.Monad.Hefty.Concurrent.Stream
import Control.Monad.Hefty.Concurrent.Timer
import Control.Monad.Hefty.Except
import Control.Monad.Hefty.Unlift
import Control.Arrow ((>>>))
import Control.Monad (forever, void, when)
import Data.Foldable (for_)
import UnliftIO (bracket_)

-- | Generates a sequence of 1, 2, 3, 4 at 0.5-second intervals.
produce :: (Output Int :> es, Timer :> es, FOEs es) => Eff es ()
produce = void . runThrow @() $
    for_ [1 ..] \(i :: Int) -> do
        when (i == 5) $ throw ()
        output i
        sleep 0.5

-- | Receives the sequence at 0.5-second intervals and prints it.
consume :: (Input Int :> es, Timer :> es, Emb IO :> es) => Eff es ()
consume = forever do
    liftIO . print =<< input @Int
    sleep 0.5

-- | Transforms by receiving the sequence as input at 0.5-second intervals,
--   adds 100, and outputs it.
plus100 :: (Input Int :> es, Output Int :> es, Timer :> es, Emb IO :> es) => Eff es ()
plus100 = forever do
    i <- input @Int
    let o = i + 100
    liftIO $ putStrLn $ "Transform " <> show i <> " to " <> show o
    output o
    sleep 0.5

main :: IO ()
main = runUnliftIO . runTimerIO $ do
    let produceWithBracket =
            bracket_
                (liftIO $ putStrLn "Start")
                (liftIO $ putStrLn "End")
                (onlyFOEs produce)

    runMachineryIO_ $
        Unit @() @Int do
            produceWithBracket
            produceWithBracket
            >>> Unit @Int @Int plus100
            >>> Unit @Int @() consume
```

```
>>> main
Start
Transform 1 to 101
101
Transform 2 to 102
102
Transform 3 to 103
103
Transform 4 to 104
104
End
Start
Transform 1 to 101
101
Transform 2 to 102
102
Transform 3 to 103
103
Transform 4 to 104
104
End
```

* Each function (machine unit) `produce`, `consume`, and `plus100` operates with input/output at 0.5-second intervals, but note that the composed stream also maintains operation intervals at 0.5 seconds (not 1.5 seconds!). This means that each unit operates concurrently based on threads.

* `End` is displayed just after the first sequence ends and before the second sequence starts. This demonstrates that the `bracket_` function based on `MonadUnliftIO` for safe resource release works in such a way that resources are released immediately at the correct timing—even if the stream is still in progress—rather than waiting until the entire stream (including the second sequence) has completed. Existing stream libraries like [`pipes`](https://hackage.haskell.org/package/pipes) and [`conduit`](https://hackage.haskell.org/package/conduit) have the issue that immediate resource release like this is not possible. This problem was first addressed by the effect system library [`bluefin`](https://github.com/tomjaguarpaw/bluefin). For more details, please refer to [Bluefin streams finalize promptly](https://h2.jaguarpaw.co.uk/posts/bluefin-streams-finalize-promptly/).

The complete code example can be found at [heftia-effects/Example/Stream/Main.hs](https://github.com/sayo-hs/heftia/blob/v0.6.0/heftia-effects/Example/Stream/Main.hs).

### Aggregating File Sizes Using Non-Deterministic Computation

The following is an extract of the main parts from an example of non-deterministic computation. For the full code, please refer to [heftia-effects/Example/NonDet/Main.hs](https://github.com/sayo-hs/heftia/blob/v0.6.0/heftia-effects/Example/NonDet/Main.hs).

```haskell
-- | Aggregate the sizes of all files under the given path
fileSizes
    :: (Choose :> es, Empty :> es, FileSystem :> es, Throw NotADir :> es, Emb IO :> es)
    => FilePath
    -> Eff es (Sum Integer)
fileSizes path = do
    entities :: [FilePath] <- listDirectory path & joinEither
    entity :: FilePath <- choice entities -- Non-deterministically /pick/ one item from the list
    let path' = path </> entity

    liftIO $ putStrLn $ "Found " <> path'

    getFileSize path' >>= \case
        Right size -> do
            liftIO $ putStrLn $ " ... " <> show size <> " bytes"
            pure $ Sum size
        Left NotAFile -> do
            fileSizes path'

main :: IO ()
main = runEff
    . runThrowIO @EntryNotFound
    . runThrowIO @NotADir
    . runDummyFS exampleRoot
    $ do
        total <- runNonDetMonoid pure (fileSizes ".")
        liftIO $ print total

-- | Effect for file system operations
data FileSystem :: Effect where
    ListDirectory :: FilePath -> FileSystem f (Either NotADir [FilePath])
    GetFileSize :: FilePath -> FileSystem f (Either NotAFile Integer)

{- |
Interpreter for the FileSystem effect that virtualizes the file system in memory
based on a given FSTree, instead of performing actual IO.
-}
runDummyFS
    :: (Throw EntryNotFound `In` es, Throw NotADir `In` es)
    => FSTree
    -> Eff (FileSystem ': es) ~> Eff es
runDummyFS root = interpret \case
    ListDirectory path ->
        lookupFS path root <&> \case
            Dir entries -> Right $ Map.keys entries
            File _ -> Left NotADir
    GetFileSize path ->
        lookupFS path root <&> \case
            File size -> Right size
            Dir _ -> Left NotAFile
```

```
>>> main
Found ./README.md
 ... 4000 bytes
Found ./src
Found ./src/Bar.hs
 ... 1000 bytes
Found ./src/Foo.hs
 ... 2000 bytes
Found ./test
Found ./test/Baz.hs
 ... 3000 bytes
Sum {getSum = 10000}
```

## Documentation
A detailed explanation of usage and semantics is available in [Haddock](https://hackage.haskell.org/package/heftia-0.6.0.0/docs/Control-Monad-Hefty.html).
The example codes are located in the [heftia-effects/Example/](https://github.com/sayo-hs/heftia/tree/v0.6.0/heftia-effects/Example) directory.
Also, the following *HeftWorld* example (outdated): https://github.com/sayo-hs/HeftWorld

About the internal *elaboration* mechanism: https://sayo-hs.github.io/jekyll/update/2024/09/04/how-the-heftia-extensible-effects-library-works.html

## Comparison

* Higher-Order Effects: Does it support higher-order effects?
* Delimited Continuation: The ability to manipulate delimited continuations.
* Effect System: For a term representing an effectful program, is it possible to statically decidable a type that enumerates all the effects the program may produce?
* Purely Monadic: Is an effectful program represented as a transparent data structure that is a monad, and can it be interpreted into other data types using only pure operations without side effects or `unsafePerformIO`?
* Dynamic Effect Rewriting: Can an effectful program have its internal effects altered afterwards (by functions typically referred to as `handle with`, `intercept`, `interpose`, `transform`, `translate`, or `rewrite`) ?

    For example, would it be possible to apply `interpose` as many times as the number of values input by the user at runtime?

* Semantics: Classification of behaviors resulting from the interpretation of effects.

    * Algebraic Effects: The same as Algebraic Effects and Handlers.
    * IO-fused: IO + ReaderT pattern.
    * Carrier dependent: The behavior depends on the specific type inference result of the monad. Tagless-final style.

| Library or Language | Higher-Order Effects | Delimited Continuation | Effect System | Purely Monadic                    | Dynamic Effect Rewriting | Semantics                        |
| ------------------- | -------------------- | ---------------------- | --------------| --------------------------------- | ------------------------ | -------------------------------- |
| `heftia`            | ✅                   | Multi-shot             | ✅            | ✅ (also `Applicative` and others)| ✅                       | Algebraic Effects                |
| `freer-simple`      | ❌                   | Multi-shot             | ✅            | ✅                                | ✅                       | Algebraic Effects                |
| `polysemy`          | ✅                   | ❌                     | ✅            | ✅                                | ✅                       | Weaving-based (functorial state) |
| `effectful`         | ✅                   | ❌                     | ✅            | ❌ (based on the `IO` monad)      | ✅                       | IO-fused                         |
| `bluefin`           | ✅[^7][^10]          | ❌                     | ✅            | ❌ (based on the `IO` monad)      | ✅[^5]                   | IO-fused                         |
| `eff`               | ✅                   | Multi-shot             | ✅            | ❌ (based on the `IO` monad)      | ✅                       | Algebraic Effects & IO-fused [^6]|
| `speff`             | ✅                   | Multi-shot (restriction: [^4]) | ✅    | ❌ (based on the `IO` monad)      | ✅                       | Algebraic Effects & IO-fused     |
| `mtl`               | ✅                   | Multi-shot (`ContT`)   | ✅            | ✅                                | ❌                       | Carrier dependent                |
| `fused-effects`     | ✅                   | ❌?                    | ✅            | ✅                                | ❌                       | Carrier dependent & Weaving-based (functorial state) |
| `in-other-words`    | ✅                   | Multi-shot?            | ✅            | ✅                                | ❌?                      | Carrier dependent                |
| Koka-lang           | ❌                   | Multi-shot             | ✅            | ❌ (language built-in)            | ✅                       | Algebraic Effects                |
| Eff-lang            | ❌                   | Multi-shot             | ✅            | ❌ (language built-in)            | ✅                       | Algebraic Effects                |
| OCaml-lang 5        | ?                    | One-shot               | ❌ [^3]       | ❌ (language built-in)            | ?                        | Algebraic Effects                |

[^3]: Effects do not appear in the type signature and can potentially cause unhandled errors at runtime
[^4]: Scoped Resumption only. e.g. Coroutines are not supported.
[^5]: https://discourse.haskell.org/t/bluefin-compared-to-effectful-video/10723/27?u=ymdfield
[^6]: https://github.com/hasura/eff/issues/12
[^7]: https://discourse.haskell.org/t/what-is-a-higher-order-effect/10744
[^10]: https://github.com/tomjaguarpaw/bluefin/pull/27

Heftia can simply be described as a higher-order version of `freer-simple`.
This is indeed true in terms of its internal mechanisms as well.

Additionally, this library provides a consistent algebraic effects semantics that is independent of carriers and effects.
On the other hand, in libraries like `in-other-words`, `mtl`, and `fused-effects`, the semantics of the code depend on the effect and, in part, the carrier inferred by type inference.
Fixing the semantics to a algebraic effects model helps improve the predictability of the behavior (interpretation result) of the code without losing flexibility.

Carrier-dependent semantics can lead to unexpected behavior for code readers, particularly in situations where the types become implicit.
Particularly, attention should be given to the fact that due to type inference, semantic changes may propagate beyond the blocks enclosed by `interpret` or `interpose`.
In the case of carrier-independent semantics, especially with Freer-based effects, `interpret` and `interpose` do not alter the semantics by intervening in type inference or instance resolution of the carrier.
Instead, they function as traditional functions, simply transforming the content of the data structure.
This results in minimal surprise to the mental model of the code reader.

### Performance

Overall, the performance of this library is positioned roughly in the middle between the fast (`effectful`, `eveff`, etc.) and slow (`polysemy`, `fused-effects`, etc.) libraries, and can be considered average.
In all benchmarks, the speed is nearly equivalent to `freer-simple`, only slightly slower.

For more details, please refer to [performance.md](https://github.com/sayo-hs/heftia/blob/v0.6.0/benchmark/performance.md).

### Interoperability with other libraries

#### About `mtl`
* Since the representation of effectful programs in Heftia is simply a monad (`Eff`), it can be used as the base monad for transformers.
    This means you can stack any transformer on top of it.

* The `Eff` monad is an instance of `MonadIO`, `MonadError`, `MonadRWS`, `MonadUnliftIO`, `Alternative`, etc., and these behave as the senders for the embedded `IO` or the effect GADTs defined in [data-effects](https://github.com/sayo-hs/data-effects).

#### About `effectful`

* In Heftia, since any monad can be used as the base monad of `Eff`, by setting the `Eff` monad from `effectful` as the base monad of Heftia, you can stack any effect in Heftia on top of `effectful`. In other words, the `Eff` of `Heftia` itself can be used like a monad transformer. This is not limited to `effectful`.

* By using `Control.Monad.Hefty.Unlift.runUnliftIO` instead of `Control.Monad.Hefty.runEff`, you can inherit and use the `MonadUnliftIO` functionality of `effectful`'s `Eff` as a higher-order `UnliftIO` effect within Heftia.

#### Representation of effects
* Heftia relies on [data-effects](https://hackage.haskell.org/package/data-effects) for the definitions of standard effects such as `Reader`, `Writer`, and `State`.

* It is generally recommended to use effects defined with automatic derivation provided by [data-effects-th](https://hackage.haskell.org/package/data-effects-th).

* The data structures used to represent effects are equivalent to those in `polysemy`, `cleff`, and `fused-effects`. However, in `heftia`, in order for operations to work, various type classes and type families (such as `HFunctor` and `OrderOf`) must also be defined for the effect types. Although it is possible to define them manually, doing so can be quite boilerplate, and to avoid the confusion caused by duplicate definitions of effects, it is recommended to use the effect types already defined in `data-effects`, and for new definitions, to use derivation via `data-effects-th`.

* It is not compatible with the structures of `effectful` and `bluefin`. A structural conversion between `effectful` and `heftia` is currently underway.

## Future Plans
* Increase effects and nurture the ecosystem
    * File systems, shell scripting, POSIX, networking, Web, markup processing/template engines, multimedia, etc.
* Further speedup
* Write practical software using Heftia
* (Support for [Linear](https://hackage.haskell.org/package/linear-base) effects?)

## License
The license is MPL 2.0. Please refer to the [NOTICE](https://github.com/sayo-hs/heftia/blob/v0.6.0/NOTICE).
Additionally, the code from `freer-simple` has been modified and used internally within this library.
Therefore, some modules are licensed under both `MPL-2.0 AND BSD-3-Clause`.
For details on licenses and copyrights, please refer to the module's Haddock documentation.

## Your contributions are welcome!
Please see [CONTRIBUTING.md](https://github.com/sayo-hs/heftia/blob/v0.6.0/CONTRIBUTING.md).

## Acknowledgements, citations, and related work
The following is a non-exhaustive list of people and works that have had a significant impact, directly or indirectly, on Heftia’s design and implementation:

- Oleg Kiselyov, Amr Sabry, and Cameron Swords — [Extensible Effects: An alternative to monad transfomers][oleg:exteff]
- Oleg Kiselyov and Hiromi Ishii — [Freer Monads, More Extensible Effects][oleg:more]
- Rob Rix, Patrick Thomson, and other contributors — [`fused-effects`][gh:fused-effects]
- Sandy Maguire and other contributors — [`polysemy`][gh:polysemy]
- Alexis King and other contributors — [`freer-simple`][gh:freer-simple], [`eff`][gh:eff]
- Casper Bach Poulsen and Cas van der Rest — [Hefty Algebras: Modular Elaboration of Higher-Order Algebraic Effects][casper:hefty]
- Tom Ellis — [Bluefin streams finalize promptly][tom:bluefin-streams]

[gh:fused-effects]: https://github.com/fused-effects/fused-effects
[gh:polysemy]: https://github.com/polysemy-research/polysemy
[oleg:exteff]: http://okmij.org/ftp/Haskell/extensible/exteff.pdf
[oleg:more]: http://okmij.org/ftp/Haskell/extensible/more.pdf
[casper:hefty]: https://dl.acm.org/doi/10.1145/3571255
[gh:freer-simple]: https://github.com/lexi-lambda/freer-simple
[gh:eff]: https://github.com/lexi-lambda/eff
[tom:bluefin-streams]: https://h2.jaguarpaw.co.uk/posts/bluefin-streams-finalize-promptly/
