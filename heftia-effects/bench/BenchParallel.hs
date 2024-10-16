{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

-- SPDX-License-Identifier: MPL-2.0

module BenchParallel where

import Control.Monad (liftM2)
import Control.Monad.Hefty (Eff, type (<:), type (<|))
import Control.Monad.Hefty.Concurrent.Parallel (runParallelIO)
import Control.Monad.Hefty.Concurrent.Stream (closing, connect)
import Control.Monad.Hefty.Input (Input, input)
import Control.Monad.Hefty.Output (Output, output)
import Control.Monad.Hefty.Unlift (runUnliftIO)
import Control.Monad.IO.Class (MonadIO)
import Data.Foldable (for_)
import Data.Maybe (fromJust)
import Data.These.Combinators (justThese)
import System.IO.Unsafe (unsafePerformIO)

produce :: (Output Int <| ef) => Int -> Eff '[] ef ()
produce n =
    for_ [1 .. n] \(i :: Int) -> do
        output i

consume :: (Input Int <: m, MonadIO m) => Int -> m [Int]
consume 0 = pure []
consume n = liftM2 (:) (input @Int) (consume (n - 1))

parallel :: Int -> [Int]
parallel n = unsafePerformIO . runUnliftIO . runParallelIO $ do
    stat <- connect @Int (produce n) (consume n)
    pure $ snd . fromJust $ justThese $ closing stat
{-# NOINLINE parallel #-}
