{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Main where

import Control.Effect.Class
import Control.Effect.Class.Machinery.HFunctor (HFunctor, hfmap)
import Control.Effect.Class.Machinery.TH
import Control.Effect.Class.Provider (ProviderS (Provide))
import Control.Effect.Class.State (State, StateI, get, gets, modify, put)
import Control.Effect.Freer
import Control.Effect.Handler.Heftia.Provider (elaborateProvider)
import Data.Effect.Class.TH
import Data.Function ((&))
import Data.Functor.Identity (Identity, runIdentity)
import Data.Vector (Vector)
import Data.Vector qualified as V

type Path = [String]

class BlockStorageF block f where
    store :: Path -> Vector block -> f ()
    load :: Path -> f (Vector block)

class WithObject block f where
    provideBlockAccess :: ProviderS (BlockAccess block) Path Identity f a -> f a

withObject ::
    forall block m a.
    (WithObject block m, Functor m) =>
    Path ->
    (forall n. BlockAccess block n => (m ~> n) -> n a) ->
    m a
withObject path m =
    fmap runIdentity $ provideBlockAccess @block $ Provide path m

class BlockAccess block f where
    appendBlock :: Vector block -> f ()
    seekBlock :: Int -> f ()
    writeBlock :: Vector block -> f ()
    readBlock :: Int -> f (Vector block)

makeEffectF ''BlockStorageF

makeSignature ''WithObject
instance HFunctor (WithObjectS block) where
    hfmap f (ProvideBlockAccess p) = ProvideBlockAccess $ hfmap f p

makeEffectF ''BlockAccess

interpretBlockAccess ::
    forall block es m.
    ( State (Vector block) (Fre es m @# "storage")
    , State Int (Fre es m @# "cursor")
    , Monad m
    ) =>
    Fre (BlockAccessI block ': es) m ~> Fre es m
interpretBlockAccess = interpret \case
    AppendBlock bs ->
        modify @(Vector block) (<> bs) & tag @"storage"
    SeekBlock cursor ->
        put cursor & tag @"cursor"
    WriteBlock bs -> do
        cursor <- get @Int & tag @"cursor"
        let n = V.length bs
        tag @"storage" $ modify @(Vector block) \bsStored ->
            V.take cursor bsStored <> bs <> V.drop (cursor + n) bsStored
        put (cursor + n) & tag @"cursor"
    ReadBlock n -> do
        cursor <- get @Int & tag @"cursor"
        gets @(Vector block) (V.take n . V.drop cursor) & tag @"storage"

elaborateWithObject :: BlockAccessI block <: es => WithObjectS block (Fre es m) ~> Fre es m
elaborateWithObject (ProvideBlockAccess p) = elaborateProvider undefined p

test :: forall block m a. Monad m => Fre '[BlockAccessI block, StateI (Vector block) # "storage", StateI Int # "cursor"] m a -> ()
test m = ()
  where
    m' = interpretBlockAccess @block m

main = pure ()
