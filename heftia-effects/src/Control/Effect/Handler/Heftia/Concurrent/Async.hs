{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Effect.Handler.Heftia.Concurrent.Async where

import Control.Concurrent (MVar, forkIO, killThread, newEmptyMVar, putMVar, readMVar)
import Control.Effect.Class (sendIns, type (<:), type (~>))
import Control.Effect.Class.Machinery.TH (makeEffect)
import Control.Effect.Freer (Fre, freerEffects, interpret, unFreerEffects)
import Control.Effect.Heftia (Elaborator)
import Control.Monad (mzero)
import Control.Monad.Trans.Cont (ContT (ContT), runContT)
import Control.Monad.Trans.Freer.Church (FreerChurchT (FreerChurchT), unFreerChurchT)
import Control.Monad.Trans.Heftia.Church (HeftiaChurchT (HeftiaChurchT), unHeftiaChurchT)
import Data.Kind (Type)
import GHC.Conc (atomically)

-- todo: add the functional dependency @f -> async@.
-- https://github.com/sayo-hs/classy-effects/issues/4

class Await (async :: Type -> Type) f where
    await :: async a -> f a

class AsyncH (async :: Type -> Type) f where
    async :: f a -> f (async a)

makeEffect "Async" ''Await ''AsyncH

asynchToIO :: Elaborator (AsyncS MVar) (Fre es IO)
asynchToIO (Async m) =
    freerEffects . FreerChurchT $ HeftiaChurchT \i -> ContT \k -> do
        let m' = unHeftiaChurchT (unFreerChurchT $ unFreerEffects m) i
        var <- newEmptyMVar
        tid <- forkIO do
            _ <- runContT m' \r -> do
                putMVar var r
                atomically mzero
            pure ()
        r <- k var
        killThread tid
        pure r

awaitToIO :: (IO <: Fre r m, Monad m) => Fre (AwaitI MVar ': r) m ~> Fre r m
awaitToIO = interpret \(Await a) -> sendIns $ readMVar a
