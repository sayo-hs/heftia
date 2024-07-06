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

## Example

Compared to existing Effect System libraries in Haskell that handle higher-order effects, this
library's approach allows for a more effortless and flexible handling of higher-order effects. Here
are some examples:

* Two interpretations of the `censor` effect for Writer

    Let's consider the following Writer effectful program:

    ```hs
    hello :: (Tell String <: m, Monad m) => m ()
    hello = do
        tell "Hello"
        tell " world!"

    censorHello :: (Tell String <: m, WriterH String <<: m, Monad m) => m ()
    censorHello =
        censor
            (\s -> if s == "Hello" then "Goodbye" else s)
            hello
    ```

    For `censorHello`, should the final written string be `"Goodbye world!"`? Or should it be `"Hello world!"`?
    With Heftia, you can freely choose either behavior depending on which higher-order effect interpreter (which we call an elaborator) you use.

    ```hs
    main :: IO ()
    main = runEff do
        (s :: String, _) <-
            interpretTell
                . interpretH (elaborateWriter @String)
                $ censorHello

        (sTransactional :: String, _) <-
            interpretTell
                . interpretH (elaborateWriterTransactional @String)
                $ censorHello

        sendIns $ putStrLn $ "Normal: " <> s
        sendIns $ putStrLn $ "Transactional: " <> sTransactional
    ```

    Using the `elaborateWriter` elaborator, you'll get "Goodbye world!", whereas with the `elaborateWriterTransactional` elaborator, you'll get "Hello world!".
    For more details, please refer to the [complete code](https://github.com/sayo-hs/heftia/blob/develop/heftia-effects/Example/Writer/Main.hs) and the [implementation of the elaborator](https://github.com/sayo-hs/heftia/blob/develop/heftia-effects/src/Control/Effect/Handler/Heftia/Writer.hs).

* Extracting Multi-shot Delimited Continuations

    In handling higher-order effects, it's easy to work with multi-shot delimited continuations.
    This enables an almost complete emulation of "Algebraic Effects and Handlers".
    For more details, please refer to
    ~~[Example 3 - Delimited Continuation](<https://github.com/sayo-hs/heftia/blob/develop/docs/examples/03%20Delimited%20Continuation.md>)~~ [an example code](heftia-effects/Example/Continuation/Main.hs) .

Furthermore, the structure of Heftia is theoretically straightforward, with ad-hoc elements being
eliminated.

Heftia is the current main focus of the [Sayo Project](https://github.com/sayo-hs).

## Documentation
~~Examples with explanations can be found in the [docs/examples/](https://github.com/sayo-hs/heftia/tree/master/docs/examples) directory.~~

Documents have become outdated.
Please wait for the documentation for the new version to be written.

The example codes are located in the [heftia-effects/Example/](heftia-effects/Example/) directory.

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
