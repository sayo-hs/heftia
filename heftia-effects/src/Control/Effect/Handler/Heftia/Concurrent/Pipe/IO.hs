-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Effect.Handler.Heftia.Concurrent.Pipe.IO where

import Control.Arrow ((>>>))
import Control.Effect (type (~>))
import Control.Effect.ExtensibleFinal (type (:!!))
import Control.Effect.Hefty (interpretRec, interpretRecH)
import Control.Lens ((^?), _Just, _Right)
import Data.Effect.Concurrent.Pipe (LPipeF, PipeF (Halt, Passthrough, Yield), PipeH (..))
import Data.Effect.Unlift (UnliftIO)
import Data.Hefty.Extensible (ForallHFunctor, type (<<|), type (<|))
import Data.Tuple (swap)
import GHC.Conc (retry)
import UnliftIO (MonadUnliftIO, atomically)
import UnliftIO.Async (withAsync)
import UnliftIO.Async qualified as Async
import UnliftIO.Concurrent qualified as Conc

runAsyncPipe ::
    forall eh ef.
    (UnliftIO <<| eh, IO <| ef, ForallHFunctor eh) =>
    PipeH ': eh :!! LPipeF ': ef ~> eh :!! ef
runAsyncPipe =
    interpretRecH \case
        PipeTo a b -> a `Async.concurrently` b
        FstWaitPipeTo a b -> a `thenStopAsync` b
        SndWaitPipeTo a b -> swap <$> b `thenStopAsync` a
        RacePipeTo a b -> a `Async.race` b
        WaitBoth a b -> a `Async.concurrently` b
        ThenStop a b -> a `thenStopAsync` b
        Race a b -> a `Async.race` b
        >>> interpretRec \case
            Passthrough -> atomically retry
            Halt -> atomically retry
            Yield -> Conc.yield

thenStopAsync :: MonadUnliftIO m => m a -> m b -> m (a, Maybe b)
thenStopAsync m1 m2 =
    withAsync m1 \a1 ->
        withAsync m2 \a2 -> do
            r1 <- Async.wait a1
            r2 <- Async.poll a2
            pure (r1, r2 ^? _Just . _Right)
