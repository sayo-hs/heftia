{-# LANGUAGE UndecidableInstances #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
    Copyright : (c) 2023 Yamada Ryo
                (c) 2023 Casper Bach Poulsen and Cas van der Rest
    License : MPL-2.0 (see the file LICENSE)

    Maintainer : ymdfield@outlook.jp
    Stability : experimental
    Portability : portable

    The data structure of hefty trees.
-}
module Control.Monad.Trans.Hefty where

import Control.Monad (ap)
import Control.Monad.Identity (Identity (Identity), runIdentity)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Trans.Free (FreeF (Free, Pure))
import Data.Functor ((<&>))

-- | A hefty monad transformer.
newtype HeftyT h m a = HeftyT {runHeftyT :: m (FreeF (h (HeftyT h m)) a (HeftyT h m a))}

instance (Functor m, Functor (h (HeftyT h m))) => Functor (HeftyT h m) where
    fmap f (HeftyT m) =
        HeftyT $
            m <&> \case
                Pure x -> Pure $ f x
                Free h -> Free $ fmap f <$> h

instance (Monad m, Functor (h (HeftyT h m))) => Applicative (HeftyT h m) where
    pure = HeftyT . pure . Pure
    (<*>) = ap

    {-# INLINE pure #-}
    {-# INLINE (<*>) #-}

instance (Monad m, Functor (h (HeftyT h m))) => Monad (HeftyT h m) where
    HeftyT m >>= k =
        HeftyT $
            m >>= \case
                Pure x -> runHeftyT $ k x
                Free h -> return $ Free $ (k =<<) <$> h

instance MonadTrans (HeftyT h) where
    lift = liftHefty
    {-# INLINE lift #-}

{- | Lift a computation to a hefty monad.

     Note that this is less constrained than MonadTrans's lift (this one only
    requires a Functor for underlying monad).
-}
liftHefty :: Functor m => m a -> HeftyT h m a
liftHefty = HeftyT . fmap Pure
{-# INLINE liftHefty #-}

-- | A hefty monad.
type Hefty h = HeftyT h Identity

hefty :: FreeF (h (Hefty h)) a (Hefty h a) -> Hefty h a
hefty = HeftyT . Identity
{-# INLINE hefty #-}

runHefty :: Hefty h a -> FreeF (h (Hefty h)) a (Hefty h a)
runHefty = runIdentity . runHeftyT
{-# INLINE runHefty #-}
