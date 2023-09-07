{-# LANGUAGE DerivingVia #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Monad.Trans.Freer.Church where

import Control.Effect.Class
import Control.Freer.Trans
import Control.Heftia.Trans (TransHeftia (..), liftSigT)
import Control.Monad.Trans
import Control.Monad.Trans.Freer
import Control.Monad.Trans.Heftia.Church

newtype FreerChurchT (ins :: Instruction) f a = FreerChurchT
    {unFreerChurchT :: HeftiaChurchT (LiftIns ins) f a}

deriving newtype instance Functor (FreerChurchT ins m)
deriving newtype instance Applicative (FreerChurchT ins m)
deriving newtype instance Monad (FreerChurchT ins m)

instance TransFreer Monad FreerChurchT where
    liftInsT = FreerChurchT . liftSigT . LiftIns
    {-# INLINE liftInsT #-}

    liftLowerFT = FreerChurchT . liftLowerHT
    {-# INLINE liftLowerFT #-}

    runInterpretF i = runElaborateH (i . unliftIns) . unFreerChurchT
    {-# INLINE runInterpretF #-}

    hoistFreer phi = FreerChurchT . hoistHeftia phi . unFreerChurchT
    {-# INLINE hoistFreer #-}

deriving via ViaLiftLower FreerChurchT ins instance MonadTrans (FreerChurchT ins)

instance MonadTransFreer FreerChurchT
