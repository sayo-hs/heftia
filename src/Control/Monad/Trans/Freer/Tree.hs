{-# LANGUAGE DerivingVia #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Monad.Trans.Freer.Tree where

import Control.Applicative (Alternative)
import Control.Effect.Class (type (~>))
import Control.Freer (Freer, interpretFF, liftIns)
import Control.Freer.Trans (FreerT (FreerT))
import Control.Monad (MonadPlus)
import Control.Monad.Base (MonadBase)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Trans (MonadIO, MonadTrans)
import Control.Monad.Trans.Free (FreeF (Free, Pure), FreeT (FreeT), MonadFree, liftF)
import Data.Functor.Coyoneda (Coyoneda (Coyoneda), liftCoyoneda)

newtype FreerTreeT m f a = FreerTreeT {unFreerTreeT :: FreeT (Coyoneda f) m a}
    deriving newtype
        ( Functor
        , Foldable
        , Applicative
        , Monad
        , Alternative
        , MonadPlus
        , MonadFree (Coyoneda f)
        , MonadBase b
        , MonadIO
        , MonadFail
        , Eq
        , Ord
        , Read
        , Show
        )
    deriving stock (Traversable)

deriving via FreeT (Coyoneda f) instance MonadTrans (FreerT FreerTreeT f)

liftInsTree :: Monad m => f a -> FreerTreeT m f a
liftInsTree = FreerTreeT . liftF . liftCoyoneda
{-# INLINE liftInsTree #-}

interpretTTree :: Monad n => (m ~> n) -> (ins ~> n) -> FreerTreeT m ins a -> n a
interpretTTree iLower i (FreerTreeT (FreeT m)) =
    iLower m >>= \case
        Pure x -> pure x
        Free (Coyoneda f e) -> i e >>= interpretTTree iLower i . FreerTreeT . f

type FreerTree = FreerTreeT Identity

instance Freer Monad FreerTree where
    liftIns = liftInsTree
    interpretFF = interpretTTree (pure . runIdentity)
    {-# INLINE liftIns #-}
    {-# INLINE interpretFF #-}
