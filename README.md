# Heftia

[![Hackage](https://img.shields.io/hackage/v/heftia.svg?logo=haskell&label=heftia)](https://hackage.haskell.org/package/heftia)
[![Hackage](https://img.shields.io/hackage/v/heftia-effects.svg?logo=haskell&label=heftia-effects)](https://hackage.haskell.org/package/heftia-effects)

Heftia is a higher-order effects version of Freer.

The paper
* Casper Bach Poulsen and Cas van der Rest. 2023. Hefty Algebras: Modular
    Elaboration of Higher-Order Algebraic Effects. Proc. ACM Program. Lang. 7,
    POPL, Article 62 (January 2023), 31 pages. <https://doi.org/10.1145/3571255>

inspires this library.
Hefty trees, proposed by the above paper, are extensions of free monads,
allowing for a straightforward treatment of higher-order effects.

This library offers Heftia monads and Freer monads, encoded into data
types in several ways to enable tuning in pursuit of high performance.

## Status

Please note that this library is currently in the experimental stage. There may be significant changes and potential bugs.

## Getting Started
To run the [Writer example](https://github.com/sayo-hs/heftia?tab=readme-ov-file#two-interpretations-of-the-censor-effect-for-writer):
```console
$ git clone https://github.com/sayo-hs/heftia
$ cd heftia/heftia-effects
$ cabal run exe:Writer
...
Pre-applying: Goodbye world!
Post-applying: Hello world!!
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
the [example code](heftia-effects/Example/Continuation/Main.hs).

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
        interpretTell
            . interpretH (elaborateWriterPre @String)
            $ censorHello

    (sPost, _) <-
        interpretTell
            . interpretH (elaborateWriterPost @String)
            $ censorHello

    liftIO $ putStrLn $ "Pre-applying: " <> sPre
    liftIO $ putStrLn $ "Post-applying: " <> sPost
```

Using the `elaborateWriterPre` elaborator, you'll get "Goodbye world!", whereas with the `elaborateWriterPost` elaborator, you'll get "Hello world!!".
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
The example codes are located in the [heftia-effects/Example/](heftia-effects/Example/) directory.
Also, the following *HeftWorld* example: https://github.com/sayo-hs/HeftWorld

~~Examples with explanations can be found in the [docs/examples/](https://github.com/sayo-hs/heftia/tree/master/docs/examples) directory.~~ Documents have become outdated.
Please wait for the documentation for the new version to be written.

## Limitation and how to avoid it
### The *reset* behavior of the scopes held by unhandled higher-order effects
When attempting to interpret an effect while there are unhandled higher-order effects present, you cannot obtain delimited continuations beyond the action scope held by these unhandled higher-order effects.
It appears as if a *reset* (in the sense of *shift/reset*) is applied to each of the scopes still held by the remaining unhandled higher-order effects.

In other words, to obtain delimited continuations that span across scopes or to maintain state across scopes,
it is necessary to first handle and eliminate all higher-order effects that hold those scopes,
and then handle the effect targeted for stateful interpretation in that order.
For this, it may be necessary to perform *multi-layering* as needed. For an example of multi-layering,
see [Example/Continuation2](https://github.com/sayo-hs/heftia/blob/f4989e92c31ae2632762afcff306ffa48c307c56/heftia-effects/Example/Continuation2/Main.hs).
For more details, please refer to the documentation of the `interpretRec` family of functions.

## Comparison

| Library or Language | Higher-Order Effects | Delimited Continuation | Statically Typed Effect Set                     | Purely Monadic                    | Dynamic Effect Rewriting | Performance (TODO) |
| ------------------- | -------------------- | ---------------------- | ----------------------------------------------- | --------------------------------- | ------------------------ | ------------------ |
| Heftia              | Yes [^1]             | Multi-shot             | Yes                                             | Yes (also Applicative and others) | Yes                      | ?                  |
| freer-simple        | No                   | Multi-shot             | Yes                                             | Yes                               | Yes                      | ?                  |
| Polysemy            | Yes                  | No                     | Yes                                             | Yes                               | Yes                      | ?                  |
| Effectful           | Yes                  | No                     | Yes                                             | No (based on the `IO` monad)      | Yes                      | ?                  |
| eff                 | Yes                  | No                     | Yes                                             | No (based on the `IO` monad)      | Yes                      | Fast               |
| mtl                 | Yes                  | Multi-shot (`ContT`)   | Yes                                             | Yes                               | No                       | ?                  |
| fused-effects       | Yes                  | No?                    | Yes                                             | Yes                               | No                       | ?                  |
| koka-lang           | No?                  | Multi-shot             | Yes                                             | No (language built-in)            | ?                        | ?                  |
| OCaml-lang 5        | Yes                  | One-shot               | No [^2]                                         | No (language built-in)            | ?                        | ?                  |

[^1]: limitation: https://github.com/sayo-hs/heftia?tab=readme-ov-file#the-reset-behavior-of-the-scopes-held-by-unhandled-higher-order-effects
[^2]: potential for 'unhandled' runtime errors

Heftia can simply be described as a higher-order version of freer-simple.
This is indeed true in terms of its internal mechanisms as well.

## Future Plans
* Benchmarking
* Enriching the documentation
* Completing missing definitions such as
    * handlers for the `Accum`, `Coroutine`, `Fresh`, `Input`, `Output` effect classes

    and others.

## License
The license is MPL 2.0. Please refer to the [NOTICE](https://github.com/sayo-hs/heftia/blob/develop/NOTICE).
Additionally, this README.md and the documents under the `docs`/`docs-ja` directory are licensed
under CC BY-SA 4.0.

## Your contributions are welcome!
Please see [CONTRIBUTING.md](https://github.com/sayo-hs/heftia/blob/develop/CONTRIBUTING.md).

## Credits
Parts of this project have been inspired by the following resources:

* **[Hefty Algebras -- The Artifact](https://github.com/heft-lang/POPL2023)**
    * **Copyright** (c) 2023 Casper Bach Poulsen and Cas van der Rest
    * **License**: MIT
