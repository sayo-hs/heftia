# Revision history for heftia-effects

## 0.1.0.0 -- 2023-09-18

* Initial public release.

## 0.2.0.0 -- 2024-07-17

* Redesign from scratch.
* Released as a beta version.

## 0.3.0.0 -- 2024-09-01

* Added interpreters for 'Timer' effects.
* Simplify the logging example.
* Update the data-effects version to 0.1.1.
* Rename the module from `Control.Effect.Handler` to `Control.Effect.Interpreter` to align with terminology.

## 0.4.0.0 -- 2024-10-10

* Support for the core package update to version 0.4.
* Dropped support for GHC 9.2.8, now supporting GHC 9.4.1 and later.
* Added benchmarks and tests.

## 0.4.0.1 -- 2024-10-14

* Fixed an issue where builds would fail with Stack: https://github.com/sayo-hs/heftia/issues/15

## 0.5.0.0 -- 2024-11-03

* **New features**
    * Added Concurrent/Parallel effects, Streaming, and Subprocess functionality.
    * Added interpreters for the `co-log` ecosystem.

* Renamed `Control.Monad.Hefty.Writer.listen` -> `intercept`
* Reexported `Data.Effect.*` from the interpreters module `Control.Monad.Hefty.*`.
* Generalized `runUnliftIO` to use any monad that is an instance of `MonadUnliftIO`.
