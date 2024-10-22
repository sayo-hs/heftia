{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

-- SPDX-License-Identifier: MPL-2.0

module Control.Monad.Hefty.Concurrent.Async (
    module Control.Monad.Hefty.Concurrent.Async,
    module Data.Effect.Concurrent.Async,
    module Data.Effect.NonDet,
)
where

import Control.Arrow ((>>>))
import Control.Exception (AsyncException (ThreadKilled), BlockedIndefinitelyOnSTM (BlockedIndefinitelyOnSTM), SomeException, fromException)
import Control.Monad (void)
import Control.Monad.Hefty (
    Eff,
    MemberBy,
    bundleN,
    interpretH,
    nil,
    unkey,
    (!+),
    (&),
    type (+),
    type (<<|),
    type (<|),
    type (~>),
 )
import Control.Monad.Hefty.Interpret (reinterpretWith)
import Control.Monad.Hefty.Unlift (UnliftIO)
import Data.Effect.Concurrent.Async
import Data.Effect.Concurrent.Parallel (Parallel)
import Data.Effect.NonDet
import Data.Effect.Unlift (withRunInIO)
import Data.Functor.Const (Const (Const))
import Data.Functor.Identity (Identity (Identity))
import Data.Set qualified as Set
import Data.Void (Void, absurd)
import Data.Word (Word8)
import Debug.Trace (trace)
import GHC.Conc (retry)
import GHC.Generics (type (:+:) (L1, R1))
import System.Random (randomIO)
import UnliftIO (
    atomically,
    catch,
    catchJust,
    finally,
    liftIO,
    mask,
    modifyTVar,
    newEmptyTMVarIO,
    newTVarIO,
    putTMVar,
    readTMVar,
    readTVarIO,
    throwIO,
    tryReadTMVar,
    uninterruptibleMask_,
 )
import UnliftIO.Concurrent (ThreadId, forkIO, killThread)

type HasAsync ef f = MemberBy AsyncKey (Async' f) ef

runAsyncSeq :: Eff '[] (Async (Const (Eff eh ef ans) + Identity) ': ef) ans -> Eff eh ef ans
runAsyncSeq =
    unkey >>> reinterpretWith \case
        Fork -> \k -> k $ Left \x ->
            L1 . Const . k . Right $
                Future
                    (R1 $ Identity x)
                    (R1 $ Identity $ Just x)
                    (R1 $ Identity ())
        Perform (L1 (Const k)) -> const k
        Perform (R1 (Identity x)) -> ($ x)

runAsyncIO :: (IO <| ef, UnliftIO <<| eh) => Eff '[] (Async (Const (IO Void) + IO) ': ef) ~> Eff eh ef
runAsyncIO m = do
    zombieThreads <- newTVarIO Set.empty

    r <-
        m
            & unkey
            & reinterpretWith \case
                Fork -> \k ->
                    withRunInIO \run -> do
                        chan <- newEmptyTMVarIO

                        mask \restore -> do
                            t <-
                                forkIO do
                                    uid <- randomIO @Word8
                                    liftIO $ putStrLn $ "Start thread " <> show uid
                                    catchJust
                                        (\e -> if fromException e == Just ThreadKilled then Nothing else Just e)
                                        ( void . restore . run $ k $ Left \s ->
                                            L1 $ Const $ do
                                                liftIO $ putStrLn $ "Performed write on " <> show uid
                                                atomically $ putTMVar chan . Right $ s
                                                atomically retry
                                        )
                                        (atomically . putTMVar chan . Left)

                            liftIO $ putStrLn $ "START " <> show t

                            atomically $ modifyTVar zombieThreads $ Set.insert t

                            let kill = do
                                    liftIO $ putStrLn $ "KILL " <> show t
                                    killThread t
                                    atomically (modifyTVar zombieThreads $ Set.delete t)

                            restore . run . k . Right $
                                Future
                                    ( R1 $
                                        traceSTMBlock t (atomically (readTMVar chan)) `finally` kill >>= \case
                                            Right r -> pure r
                                            Left (e :: SomeException) -> do
                                                throwIO e
                                    )
                                    (R1 $ atomically $ (either (const Nothing) Just =<<) <$> tryReadTMVar chan)
                                    (R1 kill)
                Perform (L1 (Const send)) -> const $ liftIO $ absurd <$> send
                Perform (R1 a) -> (liftIO a >>=)

    mapM_
        ( \t -> do
            liftIO $ putStrLn $ "KILL ZOMBIE " <> show t
            killThread t
        )
        =<< readTVarIO zombieThreads

    pure r

traceSTMBlock :: ThreadId -> IO a -> IO a
traceSTMBlock t m = do
    putStrLn $ "read " <> show t
    m `catch` \BlockedIndefinitelyOnSTM -> throwIO $ userError $ "STM blocked for read async thread " <> show t

runParallelAsync :: (HasAsync ef f) => Eff (Parallel ': eh) ef ~> Eff eh ef
runParallelAsync = interpretH parallelToAsync

runNonDetRaceIO
    :: (IO <| ef, UnliftIO <<| eh)
    => Eff '[] (Choose ': Empty ': ef) ~> Eff eh ef
runNonDetRaceIO =
    bundleN @2
        >>> reinterpretWith
            ( ( \Choose k -> withRunInIO \run -> do
                    var <- newEmptyTMVarIO
                    mask \restore -> do
                        let runThread b = forkIO $ do
                                x <- restore $ run $ k b
                                atomically $ putTMVar var x

                        t1 <- runThread False
                        t2 <- runThread True

                        atomically (readTMVar var)
                            <* uninterruptibleMask_ (killThread t1 *> killThread t2)
              )
                !+ (\Empty _ -> liftIO $ atomically retry)
                !+ nil
            )
