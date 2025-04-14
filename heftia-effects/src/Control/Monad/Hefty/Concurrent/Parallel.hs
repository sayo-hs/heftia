{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2024 Sayo contributors
License     :  MPL-2.0 (see the LICENSE file)
Maintainer  :  ymdfield@outlook.jp

Interpreters for the [Parallel]("Data.Effect.Concurrent.Parallel") effects.
-}
module Control.Monad.Hefty.Concurrent.Parallel (
    module Control.Monad.Hefty.Concurrent.Parallel,
    module Data.Effect.Concurrent.Parallel,
    module Data.Effect.Input,
)
where

#if ( __GLASGOW_HASKELL__ < 906 )
import Control.Applicative (liftA2)
#endif
import Control.Monad (forever)
import Control.Monad.Hefty (
    Eff,
    interpret,
    interpretH,
    liftIO,
    raiseAllH,
    transform,
    type (<<|),
    type (<|),
    type (~>),
    type (~~>),
 )
import Control.Monad.Hefty.Coroutine (inputToYield, runCoroutine)
import Control.Monad.Hefty.Unlift (UnliftIO)
import Data.Effect.Concurrent.Parallel
import Data.Effect.Coroutine (Status (Continue, Done))
import Data.Effect.Input
import Data.Function (fix)
import UnliftIO (
    MonadIO,
    MonadUnliftIO,
    atomically,
    mask,
    newEmptyTMVarIO,
    putTMVar,
    readTMVar,
    tryReadTMVar,
    uninterruptibleMask_,
    withRunInIO,
 )
import UnliftIO.Concurrent (forkIO, killThread, threadDelay)

polling :: (Poll <<| eh) => Eff eh ef a -> Eff '[] (Input (Maybe a) ': ef) r -> Eff eh ef r
polling pollee poller =
    poldl
        ( \case
            Done r -> const $ pure $ Left r
            Continue () k -> fmap Right . raiseAllH . k
        )
        (raiseAllH $ runCoroutine $ transform inputToYield poller)
        pollee
