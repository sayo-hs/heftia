{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Effect.Interpreter.Heftia.Concurrent.Pipe.IO.TVar where

import Control.Effect (sendIns, type (<:), type (<<:), type (~>))
import Control.Effect.ExtensibleFinal ((:!!))
import Control.Effect.Hefty (
    interposeRec,
    interposeRecH,
    interpretRec,
    interpretRecH,
    raiseUnder3,
    raiseUnderH,
    type ($),
 )
import Control.Effect.Interpreter.Heftia.Reader (runReader)
import Control.Lens (Getter, makeLenses, (%~), (.~), (?~), (^.))
import Control.Monad (forever, when)
import Control.Monad.Free (Free (Free, Pure), foldFree)
import Control.Monad.IO.Class (MonadIO)
import Data.Effect.Concurrent.Pipe (
    Consume (..),
    Feed (..),
    LConsume,
    LFeed,
    LPipeLineF,
    PipeBranchMode (DistributePipe, SplitPipe),
    PipeF (..),
    PipeH (..),
    PipeLineF (GetPipeBranchMode, IsPipeMasked),
    PipeLineH (..),
    consume,
    feed,
    fstWaitPipeTo,
    pipeTo,
    race,
    racePipeTo,
    sndWaitPipeTo,
    thenStop,
    waitBoth,
    (*|*),
    (||*),
 )
import Data.Effect.Reader (Ask, LAsk, Local (..), asks, local)
import Data.Effect.TH (makeEffectF, (&))
import Data.Function (fix)
import Data.Hefty.Extensible (ForallHFunctor, type (<<|), type (<|))
import Data.Kind (Type)
import GHC.Conc (TVar, readTVar, retry, writeTVar)
import GHC.Generics (Generic)
import UnliftIO (atomically, newTVarIO, readTVarIO)

data TVarPipeEnv chan = TVarPipeEnv
    { _inputChan :: chan
    , _outputChan :: TVar (Free TVar chan)
    , _PrevOutputChan :: Maybe (TVar (Free TVar chan))
    , _pipeMasked :: Bool
    , _branchMode :: PipeBranchMode
    }
    deriving stock (Generic)

makeLenses ''TVarPipeEnv

data TVarPipeF chan a x where
    CreatePipe :: TVarPipeF chan a chan
    FeedTVP :: chan -> a -> TVarPipeF chan a ()
    TryFeedTVP :: chan -> a -> TVarPipeF chan a Bool
    ConsumeTVP :: chan -> TVarPipeF chan a a
    TryConsumeTVP :: chan -> TVarPipeF chan a (Maybe a)

data TVarPipeDuplicator chan a (x :: Type) where
    PipeDuplicator :: chan -> TVar (Free TVar chan) -> TVar (Free TVar chan) -> TVarPipeDuplicator chan a b

makeEffectF [''TVarPipeF, ''TVarPipeDuplicator]

{-
data TVarPipeH chan a f (x :: Type) where
    DupPipe ::
        forall chan a f x.
        chan ->
        (chan -> chan -> TVar (Free TVar chan) -> TVar (Free TVar chan) -> f x) ->
        TVarPipeH chan a f x
makeEffectH [''TVarPipeH]

interpretRecH \case
    DupPipe v f -> do
        v1 <- newEmptyMVar
        v2 <- newEmptyMVar
        tv1 <- newTVarIO $ Pure v1
        tv2 <- newTVarIO $ Pure v2
        fmap fst $
            f v1 v2 tv1 tv2 *|| forever do
                ev <- takeMVar v
                (followTVars tv1 >>= (`putMVar` ev))
                    *|* (followTVars tv2 >>= (`putMVar` ev))
-}

-- https://stackoverflow.com/a/78866884
onChangeTVarChain :: (MonadIO m, Eq a) => (a -> m ()) -> TVar (Free TVar a) -> m b
onChangeTVarChain f v =
    followTVars v >>= fix \next a -> do
        a' <- waitChangeTVarChain v a
        f a'
        next a'

waitChangeTVarChain :: (MonadIO m, Eq a) => TVar (Free TVar a) -> a -> m a
waitChangeTVarChain v a = atomically do
    a' <- foldFree readTVar =<< readTVar v
    when (a == a') retry
    pure a'

defaultPipeDuplicator ::
    forall chan a m b.
    (TVarPipeF chan a <: m, PipeH <<: m, MonadIO m) =>
    chan ->
    TVar (Free TVar chan) ->
    TVar (Free TVar chan) ->
    m b
defaultPipeDuplicator src dst1 dst2 = forever do
    event <- consumeTVP @chan @a src
    (followTVars dst1 >>= (`feedTVP` event))
        *|* (followTVars dst2 >>= (`feedTVP` event))
{-# INLINE defaultPipeDuplicator #-}

followTVars :: MonadIO m => TVar (Free TVar chan) -> m chan
followTVars tv = readTVarIO tv >>= foldFree readTVarIO
{-# INLINE followTVars #-}

getOutputChan ::
    (Ask (TVarPipeEnv chan) <: m, MonadIO m) =>
    Getter (TVarPipeEnv chan) (TVar (Free TVar chan)) ->
    m chan
getOutputChan l = asks (^. l) >>= followTVars
{-# INLINE getOutputChan #-}

retryOnTVarChainChange ::
    forall chan a m b.
    (PipeH <<: m, MonadIO m, Eq chan) =>
    (chan -> a -> m b) ->
    TVar (Free TVar chan) ->
    a ->
    m b
retryOnTVarChainChange f tv x = do
    chan0 <- followTVars tv
    flip fix chan0 \next chan -> do
        f chan x `race` waitChangeTVarChain tv chan >>= \case
            Left y -> pure y
            Right chan' -> next chan'

runPipeLineBasedTVar ::
    forall chan p eh ef.
    ( PipeH <<| eh
    , PipeF <| ef
    , IO <| ef
    , TVarPipeF chan p <| ef
    , TVarPipeDuplicator chan p <| ef
    , ForallHFunctor eh
    , Eq chan
    ) =>
    chan ->
    chan ->
    PipeLineH p ': eh :!! LPipeLineF p ': LFeed p ': LConsume p ': ef ~> eh :!! ef
runPipeLineBasedTVar defaultRx defaultTx = \a -> do
    defaultTx' <- newTVarIO $ Pure defaultTx
    a
        & raiseUnderH
        & raiseUnder3
        & interpretRec \case
            IsPipeMasked -> asks (^. pipeMasked @chan)
            GetPipeBranchMode -> asks (^. branchMode @chan)
        & interpretRecH \case
            WithPipeMask f b -> local (pipeMasked @chan %~ f) b
            WithPipeBranchMode f b -> local (branchMode @chan %~ f) b
            PipeLoop b -> do
                chan <- createPipe @chan @p
                o <- newTVarIO $ Pure chan
                local ((inputChan .~ chan) . (outputChan .~ o) . (prevOutputChan ?~ o)) b
        & applyPipeTVar
        & interposeRec \case
            Passthrough -> do
                asks (^. prevOutputChan @chan) >>= mapM_ \prevOut -> do
                    o <- asks (^. outputChan @chan)
                    atomically $ writeTVar prevOut (Free o)
                forever $ consume @p >>= feed
            e -> sendIns e
        & interpretRec \case
            TryFeed x -> (`tryFeedTVP` x) =<< getOutputChan @chan outputChan
            Feed x -> (`feedTVP` x) =<< getOutputChan @chan outputChan
        & interpretRec \case
            TryConsume -> tryConsumeTVP @chan @p =<< asks (^. inputChan)
            Consume -> consumeTVP @chan @p =<< asks (^. inputChan)
        & runReader @(TVarPipeEnv chan) (TVarPipeEnv defaultRx defaultTx' Nothing True SplitPipe)
  where
    applyPipeTVar ::
        forall eh' ef'.
        ( eh' ~ Local (TVarPipeEnv chan) ': eh
        , ef' ~ LFeed p ': LConsume p ': LAsk (TVarPipeEnv chan) ': ef
        ) =>
        eh' :!! ef' ~> eh' :!! ef'
    applyPipeTVar =
        interposeRecH \case
            PipeTo a b -> pipe pipeTo a b
            FstWaitPipeTo a b -> pipe fstWaitPipeTo a b
            SndWaitPipeTo a b -> pipe sndWaitPipeTo a b
            RacePipeTo a b -> pipe racePipeTo a b
            WaitBoth a b -> branch waitBoth a b
            ThenStop a b -> branch thenStop a b
            Race a b -> branch race a b
      where
        pipe ::
            (eh' :!! ef' $ a -> eh' :!! ef' $ b -> eh' :!! ef' $ x) ->
            eh' :!! ef' $ a ->
            eh' :!! ef' $ b ->
            eh' :!! ef' $ x
        pipe f upstream downstream = do
            mask <- asks (^. pipeMasked @chan)
            if mask
                then f upstream downstream
                else do
                    chan <- createPipe @chan @p
                    o <- newTVarIO $ Pure chan
                    f
                        (local (outputChan .~ o) upstream)
                        (local ((inputChan .~ chan) . (prevOutputChan ?~ o)) downstream)

        branch ::
            (eh' :!! ef' $ a -> eh' :!! ef' $ b -> eh' :!! ef' $ x) ->
            eh' :!! ef' $ a ->
            eh' :!! ef' $ b ->
            eh' :!! ef' $ x
        branch f a b = do
            mode <- asks (^. branchMode @chan)
            case mode of
                DistributePipe -> do
                    i <- asks (^. inputChan @chan)
                    i1 <- createPipe @chan @p
                    i2 <- createPipe @chan @p
                    ti1 <- newTVarIO $ Pure i1
                    ti2 <- newTVarIO $ Pure i2
                    fmap snd $
                        pipeDuplicator @chan @p i ti1 ti2
                            ||* f
                                (local ((inputChan .~ i1) . (prevOutputChan ?~ ti1)) a)
                                (local ((inputChan .~ i2) . (prevOutputChan ?~ ti2)) b)
                SplitPipe -> f a b
