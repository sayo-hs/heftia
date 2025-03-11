{-# HLINT ignore "Use fmap" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- SPDX-License-Identifier: MPL-2.0

module Control.Monad.Ev where

import Control.Effect (runAllEff, type (~>))
import Control.Effect qualified as D
import Control.Effect.Interpret (interpret)
import Control.Effect.Transform (raiseUnder)
import Data.Effect (Emb (Emb), getEmb)
import Data.Effect.HandlerVec (empty)
import Data.Effect.State (State (..), evalStateIORef, execStateIORef)
import Data.Function ((&))
import Data.Functor ((<&>))
import UnliftIO (MonadIO, liftIO, newIORef, readIORef, writeIORef)

type Eff = D.Eff (Emb IO)

instance Functor (Eff f) where
    fmap f (D.Eff g) = D.Eff \ev -> f <$> g ev
    {-# INLINE fmap #-}

instance Applicative (Eff f) where
    pure x = D.Eff \_ -> pure x
    D.Eff ff <*> D.Eff fm = D.Eff \ev -> ff ev <*> fm ev

    {-# INLINE pure #-}
    {-# INLINE (<*>) #-}

instance Monad (Eff f) where
    D.Eff m >>= k = D.Eff \ev -> m ev >>= runAllEff ev . k
    {-# INLINE (>>=) #-}

instance {-# OVERLAPPING #-} MonadIO (Eff f) where
    liftIO m = D.Eff \_ -> Emb m
    {-# INLINE liftIO #-}

impure :: Eff (Emb IO ': es) ~> Eff es
impure = interpret \(Emb m) -> D.Eff \_ -> Emb m
{-# INLINE impure #-}

runEvEff :: Eff '[] a -> IO a
runEvEff = getEmb . runAllEff empty
{-# INLINE runEvEff #-}

evalStateEv :: s -> Eff (State s ': es) a -> Eff es a
evalStateEv s = impure . evalStateIORef s . raiseUnder
{-# INLINE evalStateEv #-}

execStateEv :: s -> Eff (State s ': es) a -> Eff es s
execStateEv s = impure . execStateIORef s . raiseUnder
{-# INLINE execStateEv #-}

runStateEv
    :: forall s es a
     . s
    -> Eff (State s ': es) a
    -> Eff es (s, a)
runStateEv s0 m = do
    ref <- newIORef s0
    a <-
        m & interpret \case
            Get -> readIORef ref
            Put s -> writeIORef ref s
    readIORef ref <&> (,a)
{-# INLINE runStateEv #-}
