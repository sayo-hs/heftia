{-# OPTIONS_GHC -fconstraint-solver-iterations=16 #-}

-- SPDX-License-Identifier: MPL-2.0

-- This example is based on https://h2.jaguarpaw.co.uk/posts/bluefin-streams-finalize-promptly/
module Main where

import Control.Arrow ((>>>))
import Control.Monad (forever, void, when)
import Control.Monad.Hefty (Eff, Emb, FOEs, liftIO, onlyFOEs, type (:>))
import Control.Monad.Hefty.Concurrent.Parallel (runParallelIO)
import Control.Monad.Hefty.Concurrent.Stream (
    Input,
    Machinery (Unit),
    Output,
    input,
    output,
    runMachinery,
    runMachineryIO_,
 )
import Control.Monad.Hefty.Concurrent.Timer (Timer, runTimerIO, sleep)
import Control.Monad.Hefty.Except (runThrow, throw)
import Control.Monad.Hefty.Unlift (runUnliftIO)
import Data.Foldable (for_)
import UnliftIO (bracket_)

{- | In reality, this 'throw' operates independently of @bracket@...
because 'runThrow' functions under the semantics of pure algebraic effects,
it operates independently without interfering with 'IO'-level exceptions.

This function is equivalent to the following (as a result of reducing 'runThrow').

@
produce = void do
    for_ [1 .. 4] \(i :: Int) -> do
        output i
        sleep 0.5
@
-}
produce :: (Output Int :> es, Timer :> es, FOEs es) => Eff es ()
produce = void . runThrow @() $
    for_ [1 ..] \(i :: Int) -> do
        when (i == 5) $ throw ()
        output i
        sleep 0.5

consume :: (Input Int :> es, Timer :> es, Emb IO :> es) => Eff es ()
consume = forever do
    liftIO . print =<< input @Int
    sleep 0.5

plus100 :: (Input Int :> es, Output Int :> es, Timer :> es, Emb IO :> es) => Eff es ()
plus100 = forever do
    i <- input @Int
    let o = i + 100
    liftIO $ putStrLn $ "Transform " <> show i <> " to " <> show o
    output o
    sleep 0.5

{- |
The difference between `runMachinery` and `runMachineryIO` is that the former
returns a continuation at the point when the stream has paused, allowing the
stream to be resumed later by providing new inputs and handlers. In terms of
algebraic effects, this means that /non-scoped resumption/ is possible.

Conversely, the latter allows the unrestricted use of `bracket`
(`MonadUnliftIO`) internally, but resumption afterwards is not possible.
`runMachineryIO` operates with a mechanism equivalent to the [Bluefin effect library](https://hackage.haskell.org/package/bluefin-0.0.9.0/docs/Bluefin-Stream.html)
and offers the same functionality.
-}
main :: IO ()
main = runUnliftIO . runTimerIO $ do
    liftIO $ putStrLn "[Parallel effect-based (purer & non-IO-fused) machinery interpretation example]"
    _ <-
        runParallelIO . runMachinery $
            Unit @() @Int produce
                >>> Unit @Int @Int plus100
                >>> Unit @Int @() consume

    liftIO $ putStrLn "\n[IO-fused machinery interpretation example]"

    let produceWithBracket =
            bracket_
                (liftIO $ putStrLn "Acquiring resource")
                (liftIO $ putStrLn "Releasing resource")
                (onlyFOEs produce)

    runMachineryIO_ $
        Unit @() @Int do
            produceWithBracket
            produceWithBracket
            >>> Unit @Int @Int plus100
            >>> Unit @Int @() consume

{-
[Parallel effect-based (purer & non-IO-fused) machinery interpretation example]
Transform 1 to 101
101
Transform 2 to 102
102
Transform 3 to 103
103
Transform 4 to 104
104

[IO-fused machinery interpretation example]
Acquiring resource
Transform 1 to 101
101
Transform 2 to 102
102
Transform 3 to 103
103
Transform 4 to 104
104
Releasing resource
Acquiring resource
Transform 1 to 101
101
Transform 2 to 102
102
Transform 3 to 103
103
Transform 4 to 104
104
Releasing resource
-}
