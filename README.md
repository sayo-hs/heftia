# Heftia: higher-order effects done right for Haskell

[![Hackage](https://img.shields.io/hackage/v/heftia.svg?logo=haskell&label=heftia)](https://hackage.haskell.org/package/heftia)
[![Hackage](https://img.shields.io/hackage/v/heftia-effects.svg?logo=haskell&label=heftia-effects)](https://hackage.haskell.org/package/heftia-effects)

Heftia is a higher-order effects version of Freer.

This library provides "[continuation-based semantics](https://github.com/lexi-lambda/eff/blob/master/notes/semantics-zoo.md)" for higher-order effects, the same as [lexi-lambda's eff](https://github.com/lexi-lambda/eff).
Instead of using the `IO` monad to implement delimited continuations for effects, Heftia internally uses `Freer` monad.

The paper
* Casper Bach Poulsen and Cas van der Rest. 2023. Hefty Algebras: Modular
    Elaboration of Higher-Order Algebraic Effects. Proc. ACM Program. Lang. 7,
    POPL, Article 62 (January 2023), 31 pages. <https://doi.org/10.1145/3571255>

inspires this library.
Hefty trees, proposed by the above paper, are extensions of free monads,
allowing for a straightforward treatment of higher-order effects.

This library offers Hefty monads and Freer monads, encoded into data
types in several ways to enable tuning in pursuit of high performance.

## Status

This library is currently in the beta stage.
There may be significant changes and potential bugs.

**We are looking forward to your feedback!**

## Installation
1.
    ```console
    $ cabal update
    ```
2. Add `heftia-effects ^>= 0.2` and `ghc-typelits-knownnat ^>= 0.7` to the build dependencies. Enable the [ghc-typelits-knownnat](https://hackage.haskell.org/package/ghc-typelits-knownnat) plugin, `GHC2021`, and the following language extensions as needed:

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
        heftia-effects ^>= 0.2,
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

This library has been tested to work with GHC 9.2.8.

## Getting Started
To run the [SemanticsZoo example](https://github.com/sayo-hs/heftia/blob/232599fbdfaf13dcf009f40585369e0ec71fd0bd/heftia-effects/Example/SemanticsZoo/Main.hs):
```console
$ git clone https://github.com/sayo-hs/heftia
$ cd heftia/heftia-effects
$ cabal run exe:SemanticsZoo
...
# State + Except
( evalState . runThrow . runCatch $ action ) = Right True
( runThrow . evalState . runCatch $ action ) = Right True

# NonDet + Except
( runNonDet . runThrow . runCatch . runChooseH $ action1 ) = [Right True,Right False]
( runThrow . runNonDet . runCatch . runChooseH $ action1 ) = Right [True,False]
( runNonDet . runThrow . runCatch . runChooseH $ action2 ) = [Right False,Right True]
( runThrow . runNonDet . runCatch . runChooseH $ action2 ) = Right [False,True]

# NonDet + Writer
( runNonDet . runTell . elaborateWriter . runChooseH $ action ) = [(3,(3,True)),(4,(4,False))]
( runTell . runNonDet . elaborateWriter . runChooseH $ action ) = (6,[(3,True),(4,False)])

[Note] All other permutations will cause type errors.
$
```

## Example

Compared to existing Effect System libraries in Haskell that handle higher-order effects, this
library's approach allows for a more effortless and flexible handling of higher-order effects. Here
are some examples:

### Extracting Multi-shot Delimited Continuations

In handling higher-order effects, it's easy to work with **multi-shot delimited continuations**.
This enables an almost complete emulation of "Algebraic Effects and Handlers".
For more details, please refer to
the [example code](https://github.com/sayo-hs/heftia/blob/232599fbdfaf13dcf009f40585369e0ec71fd0bd/heftia-effects/Example/Continuation/Main.hs).

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
            . interpretRecH (elabWriterPre @String)
            $ censorHello

    (sPost, _) <-
        runTell
            . interpretRecH (elabWriterPost @String)
            $ censorHello

    liftIO $ putStrLn $ "Pre-applying: " <> sPre
    liftIO $ putStrLn $ "Post-applying: " <> sPost
```

Using the `elabWriterPre` elaborator, you'll get "Goodbye world!", whereas with the `elabWriterPost` elaborator, you'll get "Hello world!!".
```
Pre-applying: Goodbye world!
Post-applying: Hello world!!
```

For more details, please refer to the [complete code](https://github.com/sayo-hs/heftia/blob/develop/heftia-effects/Example/Writer/Main.hs) and the [implementation of the elaborator](https://github.com/sayo-hs/heftia/blob/develop/heftia-effects/src/Control/Effect/Handler/Heftia/Writer.hs).

Furthermore, the structure of Heftia is theoretically straightforward, with ad-hoc elements being
eliminated.

Additionally, Heftia supports not only monadic effectful programs but also **applicative effectful programs**.
This may be useful when writing concurrent effectful code.

Heftia is the current main focus of the [Sayo Project](https://github.com/sayo-hs).

## Documentation
The example codes are located in the [heftia-effects/Example/](https://github.com/sayo-hs/heftia/tree/232599fbdfaf13dcf009f40585369e0ec71fd0bd/heftia-effects/Example) directory.
Also, the following *HeftWorld* example: https://github.com/sayo-hs/HeftWorld

Examples with explanations in Japanese can be found in the [docs-ja/examples/](https://github.com/sayo-hs/heftia/tree/master/docs-ja/examples) directory.

## Limitation and how to avoid it
### The *reset* behavior of the scopes held by unhandled higher-order effects
When attempting to interpret an effect while there are unhandled higher-order effects present, you cannot obtain delimited continuations beyond the action scope held by these unhandled higher-order effects.
It appears as if a *reset* (in the sense of *shift/reset*) is applied to each of the scopes still held by the remaining unhandled higher-order effects.

In other words, to obtain delimited continuations beyond their scope, it is necessary to first handle and eliminate all higher-order effects that hold those scopes,
and then handle the effect targeted for stateful interpretation in that order.
For this purpose, it might sometimes be possible to use *multi-layering*. For an example of multi-layering,
see `handleReaderThenShift` defined in [Example/Continuation2](https://github.com/sayo-hs/heftia/blob/8f71a2d4e6125018b64cbbacd32151565a29046d/heftia-effects/Example/Continuation2/Main.hs)
(particularly, the type signature of `prog` within it).
For more details, please refer to the documentation of the `interpretRec` family of functions.

## Comparison

* Higher-Order Effects: Does it support higher-order effects?
* Delimited Continuation: The ability to manipulate delimited continuations.
* Effect System: For a term representing an effectful program, is it possible to statically decidable a type that enumerates all the effects the program may produce?
* Purely Monadic: Is an effectful program represented as a transparent data structure that is a monad, and can it be interpreted into other data types using only pure operations without side effects or `unsafePerformIO`?
* Dynamic Effect Rewriting: Can an effectful program have its internal effects altered afterwards (by functions typically referred to as `handle with`, `intercept`, `interpose`, `transform`, `translate`, or `rewrite`) ?
* Performance: Time complexity or space complexity.

| Library or Language | Higher-Order Effects | Delimited Continuation | Effect System | Purely Monadic                    | Dynamic Effect Rewriting | Performance (TODO) |
| ------------------- | -------------------- | ---------------------- | --------------| --------------------------------- | ------------------------ | ------------------ |
| Heftia              | Yes [^1]             | Multi-shot             | Yes           | Yes (also Applicative and others) | Yes                      | ?                  |
| freer-simple        | No                   | Multi-shot             | Yes           | Yes                               | Yes                      | ?                  |
| Polysemy            | Yes                  | No                     | Yes           | Yes                               | Yes                      | ?                  |
| Effectful           | Yes                  | No                     | Yes           | No (based on the `IO` monad)      | Yes                      | ?                  |
| eff                 | Yes                  | Multi-shot?            | Yes           | No (based on the `IO` monad)      | Yes                      | Fast               |
| mtl                 | Yes                  | Multi-shot (`ContT`)   | Yes           | Yes                               | No                       | ?                  |
| fused-effects       | Yes                  | No?                    | Yes           | Yes                               | No                       | ?                  |
| koka-lang           | No [^2]              | Multi-shot             | Yes           | No (language built-in)            | Yes                      | ?                  |
| OCaml-lang 5        | ?                    | One-shot               | No [^3]       | No (language built-in)            | ?                        | ?                  |

[^1]: limitation: https://github.com/sayo-hs/heftia?tab=readme-ov-file#the-reset-behavior-of-the-scopes-held-by-unhandled-higher-order-effects
[^2]: https://gist.github.com/ymdryo/6fb2f7f4020c6fcda98ccc67c090dc75
[^3]: Effects do not appear in the type signature and can potentially cause unhandled errors at runtime

Heftia can simply be described as a higher-order version of freer-simple.
This is indeed true in terms of its internal mechanisms as well.

### Compatibility with other libraries
#### Representation of effects
* Heftia Effects relies on [data-effects](https://github.com/sayo-hs/data-effects) for the definitions of standard effects such as `Reader`, `Writer`, and `State`.

* It is generally recommended to use effects defined with automatic derivation provided by [data-effects-th](https://github.com/sayo-hs/data-effects/tree/develop/data-effects-th).

* The representation of first-order effects is compatible with freer-simple.
    Therefore, effects defined for freer-simple can be used as is in this library.
    However, to avoid confusion between redundantly defined effects,
    it is recommended to use the effects defined in [data-effects](https://github.com/sayo-hs/data-effects).

* GADTs for higher-order effects need to be instances of the [HFunctor](https://hackage.haskell.org/package/compdata-0.13.1/docs/Data-Comp-Multi-HFunctor.html#t:HFunctor) type class for convenient usage.
    While it is still possible to use them without being instances of `HFunctor`,
    the `interpretRec` family of functions cannot be used when higher-order effects that are not `HFunctor` are unhandled.
    If this issue is not a concern, the GADT representation of higher-order effects is compatible with Polysemy and fused-effects.
    It is not compatible with Effectful and eff.

#### About mtl
* Since the representation of effectful programs in Heftia is simply a monad (`Eff`), it can be used as the base monad for transformers.
    This means you can stack any transformer on top of it.

* The `Eff` monad is an instance of `MonadIO`, `MonadError`, `MonadRWS`, etc., and these behave as the senders for the embedded `IO` or the effect GADTs defined in [data-effects](https://github.com/sayo-hs/data-effects).

## Future Plans
* Enriching the documentation and tests
* Completing missing definitions such as
    * more patterns of interpret & transform function-families.
    * handlers for the `Accum` and others effect classes

    and others.
* Benchmarking

## License
The license is MPL 2.0. Please refer to the [NOTICE](https://github.com/sayo-hs/heftia/blob/232599fbdfaf13dcf009f40585369e0ec71fd0bd/NOTICE).
Additionally, this README.md and the documents under the `docs-ja` directory are licensed
under CC BY-SA 4.0.

## Your contributions are welcome!
Please see [CONTRIBUTING.md](https://github.com/sayo-hs/heftia/blob/232599fbdfaf13dcf009f40585369e0ec71fd0bd/CONTRIBUTING.md).

## Credits
Parts of this project have been inspired by the following resources:

* **[Hefty Algebras -- The Artifact](https://github.com/heft-lang/POPL2023)**
    * **Copyright** (c) 2023 Casper Bach Poulsen and Cas van der Rest
    * **License**: MIT
