-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Monad.Trans.Heftia.Church where

import Control.Effect.Class (type (~>))
import Control.Effect.Class.Machinery.HFunctor (hfmap)
import Control.Heftia.Trans (TransHeftia (..))
import Control.Monad (join)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Trans.Cont (ContT (ContT), runContT)
import Control.Monad.Trans.Heftia (MonadTransHeftia, elaborateMK, reelaborateMK)

newtype HeftiaChurchT h f a = HeftiaChurchT
    {unHeftiaChurchT :: forall r. (h (HeftiaChurchT h f) ~> ContT r f) -> ContT r f a}
    deriving stock (Functor)

runHeftiaChurchT :: (h (HeftiaChurchT h f) ~> ContT r f) -> HeftiaChurchT h f b -> ContT r f b
runHeftiaChurchT i (HeftiaChurchT f) = f i

instance Applicative (HeftiaChurchT h f) where
    pure x = HeftiaChurchT \_ -> pure x
    {-# INLINE pure #-}

    HeftiaChurchT f <*> HeftiaChurchT g = HeftiaChurchT \i -> f i <*> g i
    {-# INLINE (<*>) #-}

instance Monad (HeftiaChurchT h f) where
    HeftiaChurchT f >>= k =
        HeftiaChurchT \i -> f i >>= runHeftiaChurchT i . k
    {-# INLINE (>>=) #-}

instance TransHeftia Monad HeftiaChurchT where
    liftSigT e = HeftiaChurchT \i -> i e
    {-# INLINE liftSigT #-}

    translateT phi (HeftiaChurchT f) =
        HeftiaChurchT \i ->
            f $ i . phi . hfmap (translateT phi)

    liftLowerHT a = HeftiaChurchT \_ -> lift a
    {-# INLINE liftLowerHT #-}

    hoistHeftia phi (HeftiaChurchT f) =
        HeftiaChurchT \i ->
            ContT \k ->
                join . phi $
                    runContT
                        ( f \e -> ContT \k' ->
                            pure $ runContT (i $ hfmap (hoistHeftia phi) e) (join . phi . k')
                        )
                        (pure . k)

    runElaborateH g (HeftiaChurchT f) =
        runContT (f $ lift . g . hfmap (runElaborateH g)) pure

instance MonadTrans (HeftiaChurchT h) where
    lift m = HeftiaChurchT \_ -> lift m
    {-# INLINE lift #-}

instance MonadTransHeftia HeftiaChurchT where
    elaborateMK f (HeftiaChurchT g) = g $ f . hfmap (elaborateMK f)
    {-# INLINE elaborateMK #-}

    reelaborateMK f = elaborateMK f . hoistHeftia liftLowerHT
    {-# INLINE reelaborateMK #-}
