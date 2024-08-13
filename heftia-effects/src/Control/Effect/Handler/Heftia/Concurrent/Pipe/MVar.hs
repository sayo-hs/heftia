{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Effect.Handler.Heftia.Concurrent.Pipe.MVar where

import Control.Applicative (empty)
import Control.Arrow ((>>>))
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, readMVar, tryPutMVar, tryReadMVar)
import Control.Effect (type (<<:), type (~>))
import Control.Effect.ExtensibleFinal ((:!!))
import Control.Effect.Handler.Heftia.Reader (runReader)
import Control.Effect.Hefty (
    interposeRec,
    interposeRecH,
    interpretRec,
    interpretRecH,
    raiseUnder2,
    raiseUnder3,
    raiseUnderH,
    type ($),
 )
import Control.Lens (makeLenses, (.~), (?~), (^.))
import Control.Monad (forever, liftM2)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Effect.Concurrent.Pipe (
    Consume (..),
    Feed (..),
    LConsume,
    LFeed,
    LPipeLineF,
    PipeF (..),
    PipeH (..),
    PipeLine (..),
    PipeLineF (IsPipeMasked),
    Yield,
    defaultPassthrough,
    fstWaitPipeTo,
    pipeTo,
    race,
    racePipeTo,
    sndWaitPipeTo,
    thenStop,
    waitBoth,
    (*|*),
    (*||),
 )
import Data.Effect.Reader (Ask (..), Local (..), asks, local)
import Data.Function ((&))
import Data.Hefty.Extensible (ForallHFunctor, type (<<|), type (<|))
import GHC.Conc (TVar, atomically)
import GHC.Generics (Generic)

data MVarPipeEnv p = MVarPipeEnv
    { _inputMVar :: Maybe (MVar p)
    , _outputMVar :: Maybe (MVar p)
    , _pipeMasked :: Bool
    }
    deriving stock (Generic)
makeLenses ''MVarPipeEnv

runMVarPipeLine ::
    forall p eh ef.
    (PipeH <<| eh, PipeF <| ef, Yield <| ef, IO <| ef, ForallHFunctor eh) =>
    PipeLine p ': eh :!! LPipeLineF p ': LFeed p ': LConsume p ': ef ~> eh :!! ef
runMVarPipeLine =
    (raiseUnderH >>> raiseUnder3)
        >>> interpretRec \case
            IsPipeMasked -> asks $ _pipeMasked @p
        >>> interpretRecH \case
            UnmaskPipe a -> local (pipeMasked @p .~ False) a
            MaskPipe a -> local (pipeMasked @p .~ True) a
            PipeLoop a -> do
                v <- newEmptyMVar @p & liftIO
                local ((inputMVar ?~ v) . (outputMVar ?~ v)) a
        >>> applyPipeMVar @p
        >>> interpretRec \case
            TryFeed x -> maybe (pure True) (liftIO . (`tryPutMVar` x)) =<< asks (^. outputMVar)
            Feed x -> mapM_ (liftIO . (`putMVar` x)) =<< asks (^. outputMVar)
        >>> interpretRec \case
            TryConsume -> maybe (pure Nothing) (liftIO . tryReadMVar) =<< asks (^. inputMVar @p)
            Consume -> liftIO . maybe (atomically empty) readMVar =<< asks (^. inputMVar @p)
        >>> runReader (MVarPipeEnv @p Nothing Nothing True)

applyPipeMVar ::
    forall p eh ef.
    ( PipeH <<| eh
    , PipeF <| ef
    , Yield <| ef
    , Feed p <| ef
    , Consume p <| ef
    , Local (MVarPipeEnv p) <<| eh
    , Ask (MVarPipeEnv p) <| ef
    , IO <| ef
    , ForallHFunctor eh
    ) =>
    eh :!! ef ~> eh :!! ef
applyPipeMVar =
    interposeRecH \case
        PipeTo a b -> pipe pipeTo a b
        FstWaitPipeTo a b -> pipe fstWaitPipeTo a b
        SndWaitPipeTo a b -> pipe sndWaitPipeTo a b
        RacePipeTo a b -> pipe racePipeTo a b
        WaitBoth d a b -> branch d (waitBoth d) a b
        ThenStop d a b -> branch d (thenStop d) a b
        Race d a b -> branch d (race d) a b
        >>> interposeRec \Passthrough -> defaultPassthrough @p
  where
    pipe ::
        (eh :!! ef $ a -> eh :!! ef $ b -> eh :!! ef $ x) ->
        eh :!! ef $ a ->
        eh :!! ef $ b ->
        eh :!! ef $ x
    pipe f upstream downstream = do
        mask <- asks (^. pipeMasked @p)
        if mask
            then f upstream downstream
            else do
                v <- newEmptyMVar @p & liftIO
                f
                    (local (outputMVar ?~ v) upstream)
                    (local (inputMVar ?~ v) downstream)

    branch ::
        Bool ->
        (eh :!! ef $ a -> eh :!! ef $ b -> eh :!! ef $ x) ->
        eh :!! ef $ a ->
        eh :!! ef $ b ->
        eh :!! ef $ x
    branch doesDistribute f a b = do
        mask <- asks (^. pipeMasked @p)
        if doesDistribute && not mask
            then do
                iv <- asks (^. inputMVar @p)
                ov <- asks (^. outputMVar @p)
                dupMVarMay iv \ivs ->
                    dupMVarMay ov \ovs ->
                        f
                            (local ((inputMVar .~ (fst <$> ivs)) . (outputMVar .~ (fst <$> ovs))) a)
                            (local ((inputMVar .~ (snd <$> ivs)) . (outputMVar .~ (snd <$> ovs))) b)
            else f a b

    dupMVarMay :: (MonadIO m, PipeH <<: m) => Maybe (MVar a) -> (Maybe (MVar a, MVar a) -> m b) -> m b
    dupMVarMay = maybe ($ Nothing) ((. (. Just)) . dupMVar)

    dupMVar :: (MonadIO m, PipeH <<: m) => MVar a -> ((MVar a, MVar a) -> m b) -> m b
    dupMVar v f = do
        (v1, v2) <- liftIO $ liftM2 (,) newEmptyMVar newEmptyMVar
        fmap fst $
            f (v1, v2) *|| forever do
                ev <- liftIO $ readMVar v
                liftIO (putMVar v1 ev) *|* liftIO (putMVar v2 ev)
