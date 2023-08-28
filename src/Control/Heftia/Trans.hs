{-# LANGUAGE QuantifiedConstraints #-}

module Control.Heftia.Trans where

import Control.Heftia (Heftia, liftSig)
import Control.Hefty (HFunctor, hmap)
import Control.Monad.Identity (IdentityT (IdentityT), runIdentityT)
import Control.Natural (type (~>))

class (forall m. c m => Heftia c (h m)) => TransHeftia c h | h -> c where
    {-# MINIMAL liftLower, (hoistHeftia, interpretR | interpretT) #-}

    liftLower :: forall sig m a. (c m, HFunctor sig) => m a -> h m sig a

    -- | Translate an underlying monad.
    hoistHeftia :: (c m, c n, HFunctor sig) => (m ~> n) -> h m sig a -> h n sig a
    hoistHeftia phi = interpretT (liftLower . phi) (liftSig @c)
    {-# INLINE hoistHeftia #-}

    interpretR :: (c m, HFunctor sig) => (sig m ~> m) -> h m sig a -> m a
    default interpretR :: (c m, c (IdentityT m), HFunctor sig) => (sig m ~> m) -> h m sig a -> m a
    interpretR f = runIdentityT . interpretT IdentityT (IdentityT . f . hmap runIdentityT)
    {-# INLINE interpretR #-}

    interpretT :: (c m, c n, HFunctor sig) => (m ~> n) -> (sig n ~> n) -> h m sig a -> n a
    interpretT phi i = interpretR i . hoistHeftia phi
    {-# INLINE interpretT #-}
