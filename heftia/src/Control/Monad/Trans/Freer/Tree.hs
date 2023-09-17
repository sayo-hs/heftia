{-# LANGUAGE DerivingVia #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2023 Yamada Ryo
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable

A tree-structured encoded Freer transformer.
-}
module Control.Monad.Trans.Freer.Tree where

import Control.Applicative (Alternative)
import Control.Effect.Class (type (~>))
import Control.Freer (Freer, interpretF, liftIns)
import Control.Monad (MonadPlus)
import Control.Monad.Base (MonadBase)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Trans (MonadIO, MonadTrans)
import Control.Monad.Trans.Free (FreeF (Free, Pure), FreeT (FreeT), MonadFree, liftF)
import Data.Functor.Coyoneda (Coyoneda (Coyoneda), liftCoyoneda)

-- | A tree-structured encoded Freer transformer.
newtype FreerTreeT f m a = FreerTreeT {unFreerTreeT :: FreeT (Coyoneda f) m a}
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , Alternative
        , MonadPlus
        , MonadBase b
        , MonadIO
        , MonadFail
        , Eq
        , Ord
        , Read
        , Show
        , MonadTrans
        )
    deriving stock (Foldable, Traversable)

newtype FreerTreeMonad m f a = FreerTreeMonad {unFreerTreeMonad :: FreerTreeT f m a}
    deriving newtype
        ( Functor
        , Applicative
        , Alternative
        , Monad
        , MonadPlus
        , MonadBase b
        , MonadIO
        , MonadFail
        , Eq
        , Ord
        , Read
        , Show
        )
    deriving stock (Foldable, Traversable)
    deriving (MonadFree (Coyoneda f)) via FreeT (Coyoneda f) m

liftInsTree :: Monad m => ins a -> FreerTreeT ins m a
liftInsTree = FreerTreeT . liftF . liftCoyoneda
{-# INLINE liftInsTree #-}

interpretTTree :: Monad n => (m ~> n) -> (ins ~> n) -> FreerTreeT ins m a -> n a
interpretTTree iLower i (FreerTreeT (FreeT m)) =
    iLower m >>= \case
        Pure x -> pure x
        Free (Coyoneda f e) -> i e >>= interpretTTree iLower i . FreerTreeT . f

type FreerTree = FreerTreeMonad Identity

instance Freer Monad FreerTree where
    liftIns = FreerTreeMonad . liftInsTree
    interpretF i = interpretTTree (pure . runIdentity) i . unFreerTreeMonad
    {-# INLINE liftIns #-}
    {-# INLINE interpretF #-}

-- todo: MonadTransFreer instance
