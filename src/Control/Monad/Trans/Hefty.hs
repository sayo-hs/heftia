{-# LANGUAGE UndecidableInstances #-}

{- |
    Copyright : (c) 2023 Yamada Ryo
                (c) 2023 Casper Bach Poulsen and Cas van der Rest
    License : LGPL-3.0-or-later (see the file LICENSE)

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

instance (Monad m, Functor (h (HeftyT h m))) => Monad (HeftyT h m) where
    HeftyT m >>= k =
        HeftyT $
            m >>= \case
                Pure x -> runHeftyT $ k x
                Free h -> return $ Free $ (k =<<) <$> h

instance MonadTrans (HeftyT h) where
    lift = liftHefty

{- | Lift a computation to a hefty monad.

     Note that this is less constrained than MonadTrans's lift (this one only
    requires a Functor for underlying monad).
-}
liftHefty :: Functor m => m a -> HeftyT h m a
liftHefty = HeftyT . fmap Pure

-- | A hefty monad.
type Hefty h = HeftyT h Identity

hefty :: FreeF (h (Hefty h)) a (Hefty h a) -> Hefty h a
hefty = HeftyT . Identity

runHefty :: Hefty h a -> FreeF (h (Hefty h)) a (Hefty h a)
runHefty = runIdentity . runHeftyT
