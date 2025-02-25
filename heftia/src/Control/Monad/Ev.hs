{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use fmap" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Avoid lambda" #-}

-- SPDX-License-Identifier: MPL-2.0

module Control.Monad.Ev where

import Control.Effect (Free, type (~>))
import Control.Effect qualified as D
import Control.Effect.Interpret (interpret)
import Control.Effect.Transform (raiseUnder)
import Data.Effect (Emb (Emb), UnliftIO)
import Data.Effect.OpenUnion (Union, nil)
import Data.Effect.State (State, evalStateIORef, execStateIORef, runStateIORef)
import Data.Effect.Unlift (pattern WithRunInIO)
import UnliftIO (MonadIO, MonadUnliftIO, liftIO, withRunInIO)

newtype Ev f a = Ev {unEv :: (f ~> IO) -> IO a}

runEv :: (f ~> IO) -> Ev f a -> IO a
runEv hdl = (`unEv` hdl)
{-# INLINE runEv #-}

instance Functor (Ev f) where
    fmap f (Ev g) = Ev \ev -> f <$> g ev
    {-# INLINE fmap #-}

instance Applicative (Ev f) where
    pure x = Ev \_ -> pure x
    Ev ff <*> Ev fm = Ev \ev -> ff ev <*> fm ev

    {-# INLINE pure #-}
    {-# INLINE (<*>) #-}

instance Monad (Ev f) where
    Ev m >>= k = Ev \ev -> m ev >>= (`unEv` ev) . k
    {-# INLINE (>>=) #-}

instance MonadIO (Ev f) where
    liftIO m = Ev \_ -> m
    {-# INLINE liftIO #-}

instance MonadUnliftIO (Ev f) where
    withRunInIO inner = Ev \hdl -> inner $ runEv hdl
    {-# INLINE withRunInIO #-}

instance Free MonadUnliftIO Ev where
    liftFree f = Ev \hdl -> hdl f
    retract (Ev f) = withRunInIO \run -> f run
    hoist phi (Ev f) = Ev \hdl -> f $ hdl . phi
    {-# INLINE liftFree #-}
    {-# INLINE retract #-}
    {-# INLINE hoist #-}

type Eff = D.Eff Ev

impure :: Eff (Emb IO ': es) ~> Eff es
impure = interpret \(Emb m) -> D.Eff $ Ev \_ -> m
{-# INLINE impure #-}

impureUnlift :: Eff (UnliftIO ': es) ~> Eff es
impureUnlift =
    interpret \(WithRunInIO f) ->
        D.Eff $ Ev $ \hdl -> f $ interpretAllEv hdl
{-# INLINE impureUnlift #-}

interpretAllEv :: (Union es (Eff es) ~> IO) -> Eff es a -> IO a
interpretAllEv hdl = runEv hdl . D.unEff
{-# INLINE interpretAllEv #-}

runEvEff :: Eff '[] a -> IO a
runEvEff = interpretAllEv nil
{-# INLINE runEvEff #-}

runStateEv :: s -> Eff (State s ': es) a -> Eff es (s, a)
runStateEv s = impure . runStateIORef s . raiseUnder
{-# INLINE runStateEv #-}

evalStateEv :: s -> Eff (State s ': es) a -> Eff es a
evalStateEv s = impure . evalStateIORef s . raiseUnder
{-# INLINE evalStateEv #-}

execStateEv :: s -> Eff (State s ': es) a -> Eff es s
execStateEv s = impure . execStateIORef s . raiseUnder
{-# INLINE execStateEv #-}
