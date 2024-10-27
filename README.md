# Heftia: higher-order algebraic effects done right

[![Hackage](https://img.shields.io/hackage/v/heftia.svg?logo=haskell&label=heftia)](https://hackage.haskell.org/package/heftia)
[![Hackage](https://img.shields.io/hackage/v/heftia-effects.svg?logo=haskell&label=heftia-effects)](https://hackage.haskell.org/package/heftia-effects)
[![Stackage](https://www.stackage.org/package/heftia-effects/badge/nightly?label=Stackage)](https://www.stackage.org/package/heftia-effects)
[![Build status](https://img.shields.io/github/actions/workflow/status/sayo-hs/heftia/haskell.yml?branch=develop)](https://github.com/sayo-hs/heftia/actions)

Heftia is an extensible effects library for Haskell that generalizes "Algebraic Effects and Handlers" to higher-order effects, providing users with maximum flexibility and delivering standard and reasonable speed.
In its generalization, the focus is on ensuring predictable results based on simple, consistent semantics, while preserving soundness.

Please refer to the [Haddock documentation](https://hackage.haskell.org/package/heftia-0.4.0.0/docs/Control-Monad-Hefty.html) for usage and semantics.
For information on performance, please refer to [performance.md](https://github.com/sayo-hs/heftia/blob/v0.5.0/benchmark/performance.md).

This library is inspired by the paper:
* Casper Bach Poulsen and Cas van der Rest. 2023. Hefty Algebras: Modular
    Elaboration of Higher-Order Algebraic Effects. Proc. ACM Program. Lang. 7,
    POPL, Article 62 (January 2023), 31 pages. <https://doi.org/10.1145/3571255>

The *elaboration* approach proposed in the above paper allows for a straightforward treatment of higher-order effects.

Heftia's data structure is an extension of the Freer monad, designed to be theoretically straightforward by eliminating ad-hoc elements.

## Why choose this library over others?
This library is based on algebraic effects. Currently, **none of the practical effect libraries other than this one are "algebraic."** So, why is being *algebraic* important?

For example, algebraic effects are essential for managing coroutines, generators, streaming, concurrency, non-deterministic computations, and more in a highly elegant and concise manner.

Algebraic effects provide a consistent and predictable framework for handling side effects, enhancing modularity and flexibility in your code.
Research in cutting-edge languages like [Koka](https://koka-lang.github.io/koka/doc/index.html), [Eff lang](https://www.eff-lang.org/), and [OCaml 5](https://ocaml.org/manual/effects.html) is advancing the understanding and implementation of algebraic effects, establishing them as **the programming paradigm of the future**.

Heftia extends this by supporting higher-order algebraic effects, allowing for more expressive and modular effect management.
This leads to more maintainable and extensible applications compared to non-algebraic effect libraries, positioning Heftia at **the forefront of modern effect handling techniques**.

Furthermore, **Heftia is functionally a superset of other effect libraries**, especially those based on IO.
In other words, anything that is possible with other libraries is also possible with this library.
This is because Heftia supports `MonadUnliftIO` in the form of higher-order effects.

## Key Features

* **Correct Semantics for Higher-Order Effects & Continuations**

    This library provides the following features simultaneously, which existing libraries could not support together:

    * Higher-order effects
    * Delimited continuations (algebraic effects)
        * Coroutines (non-scoped resumptions)
        * Non-deterministic computations
    * [`MonadUnliftIO`](https://hackage.haskell.org/package/unliftio)
        * Code example: [heftia-effects/Example/UnliftIO/Main.hs](https://github.com/sayo-hs/heftia/blob/v0.5.0/heftia-effects/Example/UnliftIO/Main.hs)

    All of these interact through a simple, consistent, and predictable semantics based on algebraic effects.

* **Easy and Concise Implementation for Custom Effect Interpreters**

    As you can see from the implementations of basic effect interpreters such as [State](https://hackage.haskell.org/package/heftia-effects-0.4.0.0/docs/src/Control.Monad.Hefty.State.html#runState), [Throw/Catch](https://hackage.haskell.org/package/heftia-effects-0.4.0.0/docs/src/Control.Monad.Hefty.Except.html#runThrow), [Writer](https://hackage.haskell.org/package/heftia-effects-0.4.0.0/docs/src/Control.Monad.Hefty.Writer.html#runTell), [NonDet](https://hackage.haskell.org/package/heftia-effects-0.4.0.0/docs/src/Control.Monad.Hefty.NonDet.html#runNonDet), and [Coroutine](https://hackage.haskell.org/package/heftia-effects-0.4.0.0/docs/src/Control.Monad.Hefty.Coroutine.html#runCoroutine), they can be implemented in just a few lines, or even a single line. Even for effects like NonDet and Coroutine, which involve continuations and might seem difficult to implement at first glance, this is exactly how simple it can be. This is the power of algebraic effects. Users can quickly define experimental and innovative custom effects using continuations.

* **Standard and Reasonable Performance**

    It operates at a speed positioned roughly in the middle between faster libraries (like `effectful` or `eveff`) and relatively slower ones (like `polysemy` or `fused-effects`): [performance.md](https://github.com/sayo-hs/heftia/blob/v0.5.0/benchmark/performance.md).

* **Purity**

    * Does not depend on the IO monad and can use any monad as the base monad.
    * Semantics are isolated from the IO monad, meaning that aspects like asynchronous exceptions and threads do not affect the behavior of effects.
    * The constructors of the `Eff` monad are [exposed](https://hackage.haskell.org/package/heftia-0.4.0.0/docs/Control-Monad-Hefty.html#t:Eff), and users can manipulate them directly without any safety concerns. Still, the semantics remain intact.
    * These are in contrast to libraries like `effectful` and `eff`, making this library more **Haskell-ish and purely functional**.

**Heftia should be a good substitute for `mtl`, `polysemy`, `fused-effects`, and `freer-simple`.**
Additionally, if performance is not a top priority, it should also be a good alternative for `effectful`.
If performance is particularly important, [`effectful`](https://github.com/haskell-effectful/effectful) would be the best alternative to this library.

## Downsides

This library has notable semantic differences, particularly compared to libraries like `effectful`, `polysemy`, and `fused-effects`.
The semantics of this library are almost equivalent to those of `freer-simple` and are also similar to Alexis King's `eff` library.
This type of semantics is often referred to as *continuation-based semantics*.
Additionally, unlike recent libraries such as `effectful`, which have an IO-fused effect system, the semantics of this library are separated from IO.
Due to these differences, people who are already familiar with the semantics of other major libraries may find it challenging to transition to this library due to the mental model differences.

For those who have not used an extensible effects library in Haskell before, this should not be a problem.
Particularly, if you are already somewhat familiar with the semantics of algebraic effects through languages like `koka` or `eff-lang`,
you likely already have the mental model needed for this library, and everything should go smoothly.

## Status

This library is currently in the beta stage.
There may be significant changes and potential bugs.

**We are looking forward to your feedback!**

## Getting Started
1.
    ```console
    $ cabal update
    ```
2. Add `heftia-effects ^>= 0.4` and `ghc-typelits-knownnat ^>= 0.7` to the build dependencies. Enable the [ghc-typelits-knownnat](https://hackage.haskell.org/package/ghc-typelits-knownnat) plugin, `GHC2021`, and the following language extensions as needed:

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
        heftia-effects ^>= 0.4,
        ghc-typelits-knownnat ^>= 0.7,

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
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
```

to the header of your source file.

    Could not deduce ‘GHC.TypeNats.KnownNat (1 GHC.TypeNats.+ ...)’

The supported versions are GHC 9.4.1 and later.
This library has been tested with GHC 9.8.2 and 9.4.1.

## Example

Compared to existing Effect System libraries in Haskell that handle higher-order effects, this
library's approach allows for a more effortless and flexible handling of higher-order effects. Here
are some examples:

### Extracting Multi-shot Delimited Continuations

In handling higher-order effects, it's easy to work with **multi-shot delimited continuations**.
For more details, please refer to
the [example code](https://github.com/sayo-hs/heftia/blob/v0.5.0/heftia-effects/Example/Continuation/Main.hs).

### Two interpretations of the `censor` effect for Writer

Let's consider the following Writer effectful program:

```hs
hello :: (Tell String <: m, Monad m) => m ()
hello = do
    tell "Hello"
    tell " world!"

censorHello :: (Tell String <: m, WriterH String <<: m, Monad m) => m ()
censorHello =
    censor
        ( \s ->
            if s == "Hello" then
                "Goodbye"
            else if s == "Hello world!" then
                "Hello world!!"
            else
                s
        )
        hello
```

For `censorHello`, should the final written string be `"Goodbye world!"` (Pre-applying behavior) ?
Or should it be `"Hello world!!"` (Post-applying behavior) ?
With Heftia, **you can freely choose either behavior depending on which higher-order effect interpreter (which we call an elaborator) you use**.

```hs
main :: IO ()
main = runEff do
    (sPre, _) <-
        runTell
            . runWriterHPre @String
            $ censorHello

    (sPost, _) <-
        runTell
            . runWriterHPost @String
            $ censorHello

    liftIO $ putStrLn $ "Pre-applying: " <> sPre
    liftIO $ putStrLn $ "Post-applying: " <> sPost
```

Using the `runWriterHPre` elaborator, you'll get "Goodbye world!", whereas with the `runWriterHPost` elaborator, you'll get "Hello world!!".
```
Pre-applying: Goodbye world!
Post-applying: Hello world!!
```

For more details, please refer to the [complete code](https://github.com/sayo-hs/heftia/blob/v0.5.0/heftia-effects/Example/Writer/Main.hs) and the [implementation of the elaborator](https://github.com/sayo-hs/heftia/blob/v0.5.0/heftia-effects/src/Control/Monad/Hefty/Writer.hs).

### Semantics Zoo
To run the [SemanticsZoo example](https://github.com/sayo-hs/heftia/blob/v0.5.0/heftia-effects/Example/SemanticsZoo/Main.hs):
```console
$ git clone https://github.com/sayo-hs/heftia
$ cd heftia/heftia-effects
$ cabal run exe:SemanticsZoo
...
# State & Except
( evalState . runThrow . runCatch $ action ) = Right True
( runThrow . evalState . runCatch $ action ) = Right True

# NonDet & Except
( runNonDet . runThrow . runCatch . runChooseH $ action1 ) = [Right True,Right False]
( runThrow . runNonDet . runCatch . runChooseH $ action1 ) = Right [True,False]
( runNonDet . runThrow . runCatch . runChooseH $ action2 ) = [Right False,Right True]
( runThrow . runNonDet . runCatch . runChooseH $ action2 ) = Right [False,True]

# NonDet & Writer
( runNonDet . runTell . runWriterH . runChooseH $ action ) = [(3,(3,True)),(4,(4,False))]
( runTell . runNonDet . runWriterH . runChooseH $ action ) = (6,[(3,True),(4,False)])

# https://github.com/hasura/eff/issues/12
interpret SomeEff then runCatch : ( runThrow . runCatch . runSomeEff $ action ) = Right "caught"
runCatch then interpret SomeEff : ( runThrow . runSomeEff . runCatch $ action ) = Left "not caught"

[Note] All other permutations will cause type errors.
$
```

## Documentation
A detailed explanation of usage and semantics is available in [Haddock](https://hackage.haskell.org/package/heftia-0.4.0.0/docs/Control-Monad-Hefty.html).
The example codes are located in the [heftia-effects/Example/](https://github.com/sayo-hs/heftia/tree/v0.5.0/heftia-effects/Example) directory.
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
| `heftia`            | Yes                  | Multi-shot             | Yes           | Yes                               | Yes                      | Algebraic Effects                |
| `freer-simple`      | No                   | Multi-shot             | Yes           | Yes                               | Yes                      | Algebraic Effects                |
| `polysemy`          | Yes                  | No                     | Yes           | Yes                               | Yes                      | Weaving-based (functorial state) |
| `effectful`         | Yes                  | No                     | Yes           | No (based on the `IO` monad)      | Yes                      | IO-fused                         |
| `bluefin`           | Yes                  | No                     | Yes           | No (based on the `IO` monad)      | Yes                      | IO-fused                         |
| `eff`               | Yes                  | Multi-shot             | Yes           | No (based on the `IO` monad)      | Yes                      | Algebraic Effects & IO-fused [^6]|
| `speff`             | Yes                  | Multi-shot (restriction: [^4]) | Yes   | No (based on the `IO` monad)      | Yes                      | Algebraic Effects & IO-fused     |
| `in-other-words`    | Yes                  | Multi-shot?            | Yes           | Yes                               | No?                      | Carrier dependent                |
| `mtl`               | Yes                  | Multi-shot (`ContT`)   | Yes           | Yes                               | No                       | Carrier dependent                |
| `fused-effects`     | Yes                  | No?                    | Yes           | Yes                               | No                       | Carrier dependent & Weaving-based (functorial state) |
| Koka-lang           | No                   | Multi-shot             | Yes           | No (language built-in)            | Yes                      | Algebraic Effects                |
| Eff-lang            | No                   | Multi-shot             | Yes           | No (language built-in)            | Yes                      | Algebraic Effects                |
| OCaml-lang 5        | ?                    | One-shot               | No [^3]       | No (language built-in)            | ?                        | Algebraic Effects                |

[^3]: Effects do not appear in the type signature and can potentially cause unhandled errors at runtime
[^4]: Scoped Resumption only. e.g. Coroutines are not supported.
[^6]: https://github.com/hasura/eff/issues/12

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

For more details, please refer to [performance.md](https://github.com/sayo-hs/heftia/blob/v0.5.0/benchmark/performance.md).

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

* The representation of first-order effects is compatible with `freer-simple`.
    Therefore, effects defined for `freer-simple` can be used as is in this library.
    However, to avoid confusion between redundantly defined effects,
    it is recommended to use the effects defined in `data-effects`.

* GADTs for higher-order effects are formally similar to `polysemy` and `fused-effects`,
    but they need to be instances of the [`HFunctor`](https://hackage.haskell.org/package/compdata-0.13.1/docs/Data-Comp-Multi-HFunctor.html#t:HFunctor) type class.
    While it's not impossible to manually derive `HFunctor` for effect types based on these libraries and use them,
    it's inconvenient, so it's better to use `data-effects`.
    Also, it is not compatible with `effectful` and `eff`.

## Future Plans
* Increase effects and nurture the ecosystem
    * ✅ concurrent/parallel programming, streaming, `co-log`: to be released in v0.5
    * file system, Subprocesses, POSIX, and so on...
* Write practical software using Heftia
* Support for Applicative effects
* (Support for [Linear](https://hackage.haskell.org/package/linear-base) effects?)

## License
The license is MPL 2.0. Please refer to the [NOTICE](https://github.com/sayo-hs/heftia/blob/v0.5.0/NOTICE).
Additionally, the code from `freer-simple` has been modified and used internally within this library.
Therefore, some modules are licensed under both `MPL-2.0 AND BSD-3-Clause`.
For details on licenses and copyrights, please refer to the module's Haddock documentation.

## Your contributions are welcome!
Please see [CONTRIBUTING.md](https://github.com/sayo-hs/heftia/blob/v0.5.0/CONTRIBUTING.md).

## Acknowledgements, citations, and related work
The following is a non-exhaustive list of people and works that have had a significant impact, directly or indirectly, on Heftia’s design and implementation:

- Oleg Kiselyov, Amr Sabry, and Cameron Swords — [Extensible Effects: An alternative to monad transfomers][oleg:exteff]
- Oleg Kiselyov and Hiromi Ishii — [Freer Monads, More Extensible Effects][oleg:more]
- Rob Rix, Patrick Thomson, and other contributors — [`fused-effects`][gh:fused-effects]
- Sandy Maguire and other contributors — [`polysemy`][gh:polysemy]
- Alexis King and other contributors — [`freer-simple`][gh:freer-simple], [`eff`][gh:eff]
- Casper Bach Poulsen and Cas van der Rest — [Hefty Algebras: Modular Elaboration of Higher-Order Algebraic Effects][casper:hefty]

[gh:fused-effects]: https://github.com/fused-effects/fused-effects
[gh:polysemy]: https://github.com/polysemy-research/polysemy
[oleg:exteff]: http://okmij.org/ftp/Haskell/extensible/exteff.pdf
[oleg:more]: http://okmij.org/ftp/Haskell/extensible/more.pdf
[casper:hefty]: https://dl.acm.org/doi/10.1145/3571255
[gh:freer-simple]: https://github.com/lexi-lambda/freer-simple
[gh:eff]: https://github.com/lexi-lambda/eff
