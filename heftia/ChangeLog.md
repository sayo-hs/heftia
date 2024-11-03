# Revision history for heftia

## 0.1.0.0 -- 2023-09-18

* Initial public release.

## 0.2.0.0 -- 2024-07-17

* Redesign from scratch.
* Released as a beta version.

## 0.3.0.0 -- 2024-09-01

* Fixed #7: bug on `interposeRecH`. https://github.com/sayo-hs/heftia/issues/7
* Fixed #8: bug on `subsume`. https://github.com/sayo-hs/heftia/issues/8
* Added effect list manipulation functions such as `raiseNUnderM`. https://github.com/sayo-hs/heftia/issues/4
* Update the data-effects version to 0.1.1.

## 0.4.0.0 -- 2024-10-10

* Rewrote the codebase to improve performance.
    * Achieved similar speed by using techniques from freer-simple.
    * Optimized by making the open union of higher-order effects a Free HFunctor, avoiding the passing of HFunctor dictionaries.
        * Dropped support for higher-order effects that are not HFunctor.
    * Applied loopbreaker techniques to various inline functions.
    * Simplified the API by eliminating excessive generalization.
        * The interface is largely the same as before, but names have been changed throughout.
* Dropped support for GHC 9.2.8, now supporting GHC 9.4.1 and later.
* Added detailed explanations on how to use Heftia and its semantics to the Haddock documentation of the `Control.Monad.Hefty` module.

## 0.5.0.0 -- 2024-11-03

* Fixed a bug in the lookup of keyed effects.
* Added missing functions such as `key`, `keyH`, `raiseAll`, and `raiseAllH`.
