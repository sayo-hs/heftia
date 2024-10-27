{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

-- SPDX-License-Identifier: MPL-2.0

-- This example is based on https://h2.jaguarpaw.co.uk/posts/bluefin-streams-finalize-promptly/
module Main where

import Control.Arrow ((>>>))
import Control.Monad (forever, void, when)
import Control.Monad.Hefty (Eff, liftIO, raiseAllH, type (<:), type (<|))
import Control.Monad.Hefty.Concurrent.Parallel (runParallelIO)
import Control.Monad.Hefty.Concurrent.Stream (Machinery (Unit), runMachinery, runMachineryIO)
import Control.Monad.Hefty.Concurrent.Timer (Timer, runTimerIO, sleep)
import Control.Monad.Hefty.Except (runThrow, throw)
import Control.Monad.Hefty.Input (Input, input)
import Control.Monad.Hefty.Output (Output, output)
import Control.Monad.Hefty.Resource (bracket_, runResourceIO)
import Control.Monad.Hefty.Unlift (runUnliftIO)
import Control.Monad.IO.Class (MonadIO)
import Data.Foldable (for_)

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
produce :: (Output Int <| ef, Timer <| ef) => Eff '[] ef ()
produce = void . runThrow @() $ do
    for_ [1 ..] \(i :: Int) -> do
        when (i == 5) $ throw ()
        output i
        sleep 0.5

consume :: (Input Int <: m, Timer <: m, MonadIO m) => m ()
consume = forever do
    liftIO . print =<< input @Int
    sleep 0.5

plus100 :: (Input Int <: m, Output Int <: m, Timer <: m, MonadIO m) => m ()
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
main = runUnliftIO . runTimerIO . runResourceIO $ do
    liftIO $ putStrLn "[Parallel effect-based (purer & non-IO-fused) machinery interpretation example]"
    _ <-
        runParallelIO . runMachinery $
            Unit @() @Int produce
                >>> Unit @Int @Int plus100
                >>> Unit @Int @() consume

    liftIO $ putStrLn "\n[IO-fused machinery interpretation example]"

    let produceWithBracket = do
            bracket_
                (liftIO $ putStrLn "Acquiring resource")
                (liftIO $ putStrLn "Releasing resource")
                (raiseAllH produce)

    runMachineryIO (pure ()) (const $ pure ()) $
        Unit @() @Int do
            produceWithBracket
            produceWithBracket
            >>> Unit @Int @Int plus100
            >>> Unit @Int @() consume

    pure ()

{-
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
