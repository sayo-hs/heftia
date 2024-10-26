{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

-- SPDX-License-Identifier: MPL-2.0

-- This example is based on https://h2.jaguarpaw.co.uk/posts/bluefin-streams-finalize-promptly/
module Main where

import Control.Monad (forever, void, when)
import Control.Monad.Hefty (
    Eff,
    interpret,
    liftIO,
    raise,
    (&),
    type (<:),
    type (<<|),
    type (<|),
    type (~>),
 )
import Control.Monad.Hefty.Concurrent.Stream (connect)
import Control.Monad.Hefty.Concurrent.Timer (Timer, runTimerIO, sleep)
import Control.Monad.Hefty.Except (runThrow, throw)
import Control.Monad.Hefty.Input (Input, input)
import Control.Monad.Hefty.Output (Output, output)
import Control.Monad.Hefty.Resource (Resource, bracket_, runResourceIO)
import Control.Monad.Hefty.Unlift (runUnliftIO)
import Control.Monad.IO.Class (MonadIO)
import Data.Foldable (for_)

data SomeIOResource a where
    ReadResource :: SomeIOResource Int
    WriteResource :: Int -> SomeIOResource ()

runSomeResource
    :: (Resource <<| eh, IO <| ef)
    => Eff eh (SomeIOResource ': ef) ~> Eff eh ef
runSomeResource m = bracket_
    (liftIO $ putStrLn "Acquiring resource")
    (liftIO $ putStrLn "Releasing resource")
    do
        m & interpret \case
            ReadResource -> error "just dummy"
            WriteResource _ -> error "just dummy"

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
produce :: (Output Int <| ef, Timer <| ef) => Int -> Eff '[] ef ()
produce n = void . runThrow @() $ do
    for_ [1 ..] \(i :: Int) -> do
        when (i == 5) $ throw ()
        output $ n + i

-- sleep 0.5

consume :: (Input Int <: m, MonadIO m) => m ()
consume = forever do
    liftIO . print =<< input @Int

plus100 :: (Input Int <: m, Output Int <: m, MonadIO m) => m ()
plus100 = forever do
    i <- input @Int
    liftIO $ print i
    output $ i + 100

main :: IO ()
main = runUnliftIO . runTimerIO . runResourceIO $ do
    -- _ <- runSomeResource $ runAsyncIO $ connect @Int (produce 0) consume
    -- _ <- runSomeResource $ runAsyncIO $ connect @Int (connect @Int (produce 0) plus100) consume
    pure ()

{-
Acquiring resource
1
2
Releasing resource
Acquiring resource
1
2
Releasing resource
-}
