#!/bin/sh
set -e
cd heftia-effects
cabal run heftia-bench -- --svg ../benchmark/bench-result/countdown-shallow.svg --pattern '$2 == "countdown.shallow"'
cabal run heftia-bench -- --svg ../benchmark/bench-result/countdown-deep.svg --pattern '$2 == "countdown.deep"'
cabal run heftia-bench -- --svg ../benchmark/bench-result/nondet-shallow.svg --pattern '$2 == "nondet.shallow"'
cabal run heftia-bench -- --svg ../benchmark/bench-result/nondet-deep.svg --pattern '$2 == "nondet.deep"'
cabal run heftia-bench -- --svg ../benchmark/bench-result/catch-shallow.svg --pattern '$2 == "catch.shallow"'
cabal run heftia-bench -- --svg ../benchmark/bench-result/catch-deep.svg --pattern '$2 == "catch.deep"'
cabal run heftia-bench -- --svg ../benchmark/bench-result/local-shallow.svg --pattern '$2 == "local.shallow"'
cabal run heftia-bench -- --svg ../benchmark/bench-result/local-deep.svg --pattern '$2 == "local.deep"'
cabal run heftia-bench -- --svg ../benchmark/bench-result/coroutine-shallow.svg --pattern '$2 == "coroutine.shallow"'
cabal run heftia-bench -- --svg ../benchmark/bench-result/coroutine-deep.svg --pattern '$2 == "coroutine.deep"'
