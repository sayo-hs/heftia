#!/bin/sh
cd heftia-effects
cabal run heftia-bench -- --svg bench-result/countdown.svg --pattern '$2 == "countdown"'
cabal run heftia-bench -- --svg bench-result/nondet.svg --pattern '$2 == "nondet"'
cabal run heftia-bench -- --svg bench-result/catch.svg --pattern '$2 == "catch"'
cabal run heftia-bench -- --svg bench-result/coroutine.svg --pattern '$2 == "coroutine"'
