{-# LANGUAGE QuantifiedConstraints #-}

module Control.Heftier.Trans where

import Control.Heftier (Heftier, liftSig)
import Control.Hefty (HFunctor, hmap)
import Control.Monad.Identity (IdentityT (IdentityT), runIdentityT)
import Control.Natural (type (~>))

class (forall m. c m => Heftier c (h m)) => TransHeftier c h | h -> c where
    {-# MINIMAL liftLower, (hoistHeftier, interpretR | interpretT) #-}

    liftLower :: forall sig m a. (c m, HFunctor sig) => m a -> h m sig a

    -- | Translate an underlying monad.
    hoistHeftier :: (c m, c n, HFunctor sig) => (m ~> n) -> h m sig a -> h n sig a
    hoistHeftier phi = interpretT (liftLower . phi) (liftSig @c)
    {-# INLINE hoistHeftier #-}

    interpretR :: (c m, HFunctor sig) => (sig m ~> m) -> h m sig a -> m a
    default interpretR :: (c m, c (IdentityT m), HFunctor sig) => (sig m ~> m) -> h m sig a -> m a
    interpretR f = runIdentityT . interpretT IdentityT (IdentityT . f . hmap runIdentityT)
    {-# INLINE interpretR #-}

    interpretT :: (c m, c n, HFunctor sig) => (m ~> n) -> (sig n ~> n) -> h m sig a -> n a
    interpretT phi i = interpretR i . hoistHeftier phi
    {-# INLINE interpretT #-}
