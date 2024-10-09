# Heftia: higher-order effects done right for Haskell

[![Hackage](https://img.shields.io/hackage/v/heftia.svg?logo=haskell&label=heftia)](https://hackage.haskell.org/package/heftia)
[![Hackage](https://img.shields.io/hackage/v/heftia-effects.svg?logo=haskell&label=heftia-effects)](https://hackage.haskell.org/package/heftia-effects)

Heftia is an extensible effects library that generalizes Algebraic Effects and Handlers to higher-order effects, providing users with maximum flexibility and delivering standard and reasonable speed.
In its generalization, the focus is on ensuring predictable results based on simple, consistent semantics, while preserving soundness.

Please refer to the [Haddock documentation](https://hackage.haskell.org/package/heftia-0.4.0.0/docs/Control-Monad-Hefty.html) for usage and semantics.
For information on performance, please refer to [performance.md](https://github.com/sayo-hs/heftia/blob/v0.4.0/benchmark/performance.md).

The library allows the following effects with well-defined semantics:

* Coroutines
* Non-deterministic computations
* `MonadUnliftIO`

This library is inspired by the paper:
* Casper Bach Poulsen and Cas van der Rest. 2023. Hefty Algebras: Modular
    Elaboration of Higher-Order Algebraic Effects. Proc. ACM Program. Lang. 7,
    POPL, Article 62 (January 2023), 31 pages. <https://doi.org/10.1145/3571255>

The /elaboration/ approach proposed in the above paper allows for a straightforward treatment of higher-order effects.

Heftia's data structure is an extension of the Freer monad, designed to be theoretically straightforward by eliminating ad-hoc elements.

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

    ghc-options: ... -fplugin GHC.TypeLits.KnownNat.Solver
...
```

This library has been tested to work with GHC 9.8.2.

## Getting Started

## Example

Compared to existing Effect System libraries in Haskell that handle higher-order effects, this
library's approach allows for a more effortless and flexible handling of higher-order effects. Here
are some examples:

### Extracting Multi-shot Delimited Continuations

In handling higher-order effects, it's easy to work with **multi-shot delimited continuations**.
For more details, please refer to
the [example code](https://github.com/sayo-hs/heftia/blob/v0.4.0/heftia-effects/Example/Continuation/Main.hs).

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

Using the `elabWriterPre` elaborator, you'll get "Goodbye world!", whereas with the `elabWriterPost` elaborator, you'll get "Hello world!!".
```
Pre-applying: Goodbye world!
Post-applying: Hello world!!
```

For more details, please refer to the [complete code](https://github.com/sayo-hs/heftia/blob/v0.4.0/heftia-effects/Example/Writer/Main.hs) and the [implementation of the elaborator](https://github.com/sayo-hs/heftia/blob/v0.4.0/heftia-effects/src/Control/Effect/Interpreter/Heftia/Writer.hs).

### Semantics Zoo
To run the [SemanticsZoo example](https://github.com/sayo-hs/heftia/blob/08f5cfe6a8f5c0383ea2b02e93326552400f7fd3/heftia-effects/Example/SemanticsZoo/Main.hs):
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
( runNonDet . runTell . elaborateWriter . runChooseH $ action ) = [(3,(3,True)),(4,(4,False))]
( runTell . runNonDet . elaborateWriter . runChooseH $ action ) = (6,[(3,True),(4,False)])

# https://github.com/hasura/eff/issues/12
interpret SomeEff then runCatch : ( runThrow . runCatch . runSomeEff $ action ) = Right "caught"
runCatch then interpret SomeEff : ( runThrow . runSomeEff . runCatch $ action ) = Left "not caught"

[Note] All other permutations will cause type errors.
$
```

## Documentation
A detailed explanation of usage and semantics is available in [Haddock](https://hackage.haskell.org/package/heftia-0.4.0.0/docs/Control-Monad-Hefty.html).
The example codes are located in the [heftia-effects/Example/](https://github.com/sayo-hs/heftia/tree/v0.4.0/heftia-effects/Example) directory.
Also, the following *HeftWorld* example (outdated): https://github.com/sayo-hs/HeftWorld

About the internal /elaboration/ mechanism: https://sayo-hs.github.io/jekyll/update/2024/09/04/how-the-heftia-extensible-effects-library-works.html

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
| `eff`               | Yes                  | Multi-shot             | Yes           | No (based on the `IO` monad)      | Yes                      | Algebraic Effects & IO-fused [^6]|
| `speff`             | Yes                  | Multi-shot (restriction: [^4]) | Yes   | No (based on the `IO` monad)      | Yes                      | Algebraic Effects & IO-fused     |
| `in-other-words`    | Yes                  | Multi-shot?            | Yes           | Yes                               | No?                      | Carrier dependent                |
| `mtl`               | Yes                  | Multi-shot (`ContT`)   | Yes           | Yes                               | No                       | Carrier dependent                |
| `fused-effects`     | Yes                  | No?                    | Yes           | Yes                               | No                       | Carrier dependent & Weaving-based (functorial state) |
| Koka-lang           | No                   | Multi-shot             | Yes           | No (language built-in)            | Yes                      | Algebraic Effects                |
| OCaml-lang 5        | ?                    | One-shot               | No [^3]       | No (language built-in)            | ?                        | Algebraic Effects?               |

[^3]: Effects do not appear in the type signature and can potentially cause unhandled errors at runtime
[^4]: Scoped Resumption only. e.g. Coroutines are not supported.
[^5]: https://github.com/sayo-hs/heftia/issues/12
[^6]: https://github.com/hasura/eff/issues/12

Heftia can simply be described as a higher-order version of freer-simple.
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

For more details, please refer to [performance.md](https://github.com/sayo-hs/heftia/blob/v0.4.0/benchmark/performance.md).

### Compatibility with other libraries

#### About mtl
* Since the representation of effectful programs in Heftia is simply a monad (`Eff`), it can be used as the base monad for transformers.
    This means you can stack any transformer on top of it.

* The `Eff` monad is an instance of `MonadIO`, `MonadError`, `MonadRWS`, `MonadUnliftIO`, `Alternative`, etc., and these behave as the senders for the embedded `IO` or the effect GADTs defined in [data-effects](https://github.com/sayo-hs/data-effects).

#### Representation of effects
* Heftia relies on [data-effects](https://hackage.haskell.org/package/data-effects) for the definitions of standard effects such as `Reader`, `Writer`, and `State`.

* It is generally recommended to use effects defined with automatic derivation provided by [data-effects-th](https://hackage.haskell.org/package/data-effects-th).

* The representation of first-order effects is compatible with freer-simple.
    Therefore, effects defined for freer-simple can be used as is in this library.
    However, to avoid confusion between redundantly defined effects,
    it is recommended to use the effects defined in `data-effects`.

* GADTs for higher-order effects are formally similar to Polysemy and fused-effects,
    but they need to be instances of the [`HFunctor`](https://hackage.haskell.org/package/compdata-0.13.1/docs/Data-Comp-Multi-HFunctor.html#t:HFunctor) type class.
    While it's not impossible to manually derive `HFunctor` for effect types based on these libraries and use them,
    it's inconvenient, so it's better to use `data-effects`.
    Also, it is not compatible with Effectful and eff.

## Future Plans
* Support for Applicative effects.
* Completing lacking definitions such as
    * interpreters for the `Accum` and others effects

    and others.

## License
The license is MPL 2.0. Please refer to the [NOTICE](https://github.com/sayo-hs/heftia/blob/v0.4.0/NOTICE).
Additionally, this README.md and the documents under the `docs-ja` directory are licensed
under CC BY-SA 4.0.

## Your contributions are welcome!
Please see [CONTRIBUTING.md](https://github.com/sayo-hs/heftia/blob/v0.4.0/CONTRIBUTING.md).

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
