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
import Control.Effect.Transform (onlyFirstOrder)
import Control.Monad.Hefty (Eff, transform, (:>))
import Control.Monad.Hefty.Coroutine (inputToYield, runCoroutine)
import Data.Effect.Concurrent.Parallel
import Data.Effect.Coroutine (Status (Continue, Done))
import Data.Effect.Input
import Data.Effect.OpenUnion (RemoveHOEs, WeakenHOEs)

polling :: (Poll :> es, WeakenHOEs es) => Eff es a -> Eff (Input (Maybe a) ': RemoveHOEs es) a -> Eff es a
polling pollee poller =
    poldl
        ( \case
            Done r -> const $ pure $ Left r
            Continue () k -> fmap Right . onlyFirstOrder . k
        )
        (onlyFirstOrder $ runCoroutine $ transform inputToYield poller)
        pollee
