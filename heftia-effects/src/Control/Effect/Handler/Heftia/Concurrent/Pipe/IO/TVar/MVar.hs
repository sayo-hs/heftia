-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Effect.Handler.Heftia.Concurrent.Pipe.IO.TVar.MVar where

import Control.Arrow ((>>>))
import Control.Effect (type (~>))
import Control.Effect.ExtensibleFinal ((:!!))
import Control.Effect.Handler.Heftia.Concurrent.Pipe.IO.TVar (LTVarPipeDuplicator, LTVarPipeF, TVarPipeDuplicator (..), TVarPipeF (..), defaultPipeDuplicator, runPipeLineBasedTVar)
import Control.Effect.Hefty (interpretRec, raiseUnder3)
import Data.Effect.Concurrent.Pipe (
    LConsume,
    LFeed,
    LPipeLineF,
    PipeF (..),
    PipeH (..),
    PipeLineH (..),
 )
import Data.Hefty.Extensible (ForallHFunctor, type (<<|), type (<|))
import UnliftIO (
    MVar,
    newEmptyMVar,
    putMVar,
    takeMVar,
    tryPutMVar,
    tryTakeMVar,
 )

runMVarPipeLine ::
    forall p eh ef.
    ( PipeH <<| eh
    , PipeF <| ef
    , IO <| ef
    , ForallHFunctor eh
    ) =>
    MVar p ->
    MVar p ->
    PipeLineH p ': eh :!! LPipeLineF p ': LFeed p ': LConsume p ': ef ~> eh :!! ef
runMVarPipeLine defaultRx defaultTx =
    (raiseUnder3 >>> raiseUnder3)
        >>> runPipeLineBasedTVar defaultRx defaultTx
        >>> interpretRec @(LTVarPipeDuplicator (MVar p) p) \case
            PipeDuplicator src dst1 dst2 ->
                defaultPipeDuplicator @(MVar p) @p src dst1 dst2
        >>> interpretRec @(LTVarPipeF (MVar p) p) \case
            CreatePipe -> newEmptyMVar
            FeedTVP v x -> putMVar v x
            TryFeedTVP v x -> tryPutMVar v x
            ConsumeTVP v -> takeMVar v
            TryConsumeTVP v -> tryTakeMVar v
