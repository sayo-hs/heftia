-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Monad.Trans.Heftia.Church where

import Control.Effect.Class (type (~>))
import Control.Effect.Class.Machinery.HFunctor (hfmap)
import Control.Heftia.Trans (TransHeftia (..))
import Control.Monad.Trans.Cont (Cont, ContT (ContT), runCont)
import Data.Free.Sum (caseF, pattern L1, pattern R1, type (+))
import Data.Functor.Identity (Identity (Identity), runIdentity)

newtype HeftiaChurchT h f a = HeftiaChurchT
    {unHeftiaChurchT :: forall r. ((h (HeftiaChurchT h f) + f) ~> Cont r) -> Cont r a}
    deriving stock (Functor)

runHeftiaChurchT :: ((h (HeftiaChurchT h f) + f) ~> Cont r) -> HeftiaChurchT h f b -> Cont r b
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
    liftSigT e = HeftiaChurchT \i -> i $ L1 e
    {-# INLINE liftSigT #-}

    translateT phi (HeftiaChurchT f) =
        HeftiaChurchT \i ->
            f $ i . caseF (L1 . phi . hfmap (translateT phi)) R1

    liftLowerHT a = HeftiaChurchT \i -> i $ R1 a
    {-# INLINE liftLowerHT #-}

    hoistHeftia phi (HeftiaChurchT f) =
        HeftiaChurchT \i ->
            f $ i . caseF (L1 . hfmap (hoistHeftia phi)) (R1 . phi)

    runElaborateH g (HeftiaChurchT f) =
        let run m = ContT \k -> Identity $ m >>= runIdentity . k
         in runCont (f $ caseF (run . g . hfmap (runElaborateH g)) run) pure
