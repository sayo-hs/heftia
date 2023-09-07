{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Freer.Trans.Final
    {-# DEPRECATED
        "The current implementation of final-encoded Freer and Heftia can easily cause infinite loops."
        #-}
where

import Control.Applicative (Alternative)
import Control.Effect.Class (Instruction, LiftIns (LiftIns), unliftIns, type (~>))
import Control.Freer.Trans (TransFreer (..), liftInsT)
import Control.Heftia.Trans.Final (
    FinalTElaborator (FinalTElaborator),
    HeftiaFinalT,
    elaborateFinalT,
    elaborateFinalTLower,
    heftiaFinalT,
    hoistHeftiaFinal,
    liftLowerHTFinal,
    liftSigFinalT,
    runHeftiaFinalT,
    subsumeHeftiaFinal,
    weakenHeftiaFinalT,
 )
import Control.Monad (MonadPlus)
import Control.Monad.Trans (MonadTrans)
import Control.Monad.Trans.Freer (MonadTransFreer, ViaLiftLower (ViaLiftLower))

newtype FreerFinalT c (ins :: Instruction) f a = FreerFinalT
    {unFreerFinalT :: HeftiaFinalT c (LiftIns ins) f a}

data FinalTInterpreter ins f g = FinalTInterpreter
    { interpretFinalTLower :: f ~> g
    , interpretFinalT :: ins ~> g
    }

runFreerFinalT :: c g => FinalTInterpreter ins f g -> FreerFinalT c ins f ~> g
runFreerFinalT FinalTInterpreter{..} (FreerFinalT a) =
    ($ a) $ runHeftiaFinalT $ FinalTElaborator interpretFinalTLower (interpretFinalT . unliftIns)
{-# INLINE runFreerFinalT #-}

freerFinalT :: (forall g. c g => FinalTInterpreter ins f g -> g a) -> FreerFinalT c ins f a
freerFinalT f =
    FreerFinalT $ heftiaFinalT \FinalTElaborator{..} ->
        f $ FinalTInterpreter elaborateFinalTLower (elaborateFinalT . LiftIns)
{-# INLINE freerFinalT #-}

liftInsFinalT :: ins ~> FreerFinalT c ins f
liftInsFinalT = FreerFinalT . liftSigFinalT . LiftIns
{-# INLINE liftInsFinalT #-}

liftLowerFTFinal :: f ~> FreerFinalT c ins f
liftLowerFTFinal = FreerFinalT . liftLowerHTFinal
{-# INLINE liftLowerFTFinal #-}

weakenFreerFinalT :: (forall g. c' g => c g) => FreerFinalT c ins f ~> FreerFinalT c' ins f
weakenFreerFinalT = FreerFinalT . weakenHeftiaFinalT . unFreerFinalT
{-# INLINE weakenFreerFinalT #-}

hoistFreerFinal :: (f ~> g) -> FreerFinalT c ins f ~> FreerFinalT c ins g
hoistFreerFinal phi = FreerFinalT . hoistHeftiaFinal phi . unFreerFinalT
{-# INLINE hoistFreerFinal #-}

deriving newtype instance Functor (FreerFinalT Functor ins f)

deriving newtype instance Functor (FreerFinalT Applicative ins f)
deriving newtype instance Applicative (FreerFinalT Applicative ins f)

deriving newtype instance Functor (FreerFinalT Alternative ins f)
deriving newtype instance Applicative (FreerFinalT Alternative ins f)
deriving newtype instance Alternative (FreerFinalT Alternative ins f)

deriving newtype instance Functor (FreerFinalT Monad ins m)
deriving newtype instance Applicative (FreerFinalT Monad ins m)
deriving newtype instance Monad (FreerFinalT Monad ins m)

deriving newtype instance Functor (FreerFinalT MonadPlus ins m)
deriving newtype instance Applicative (FreerFinalT MonadPlus ins m)
deriving newtype instance Alternative (FreerFinalT MonadPlus ins m)
deriving newtype instance Monad (FreerFinalT MonadPlus ins m)
deriving newtype instance MonadPlus (FreerFinalT MonadPlus ins m)

instance (forall h f. c f => c (FreerFinalT c h f)) => TransFreer c (FreerFinalT c) where
    liftInsT = liftInsFinalT
    {-# INLINE liftInsT #-}

    liftLowerFT = liftLowerFTFinal
    {-# INLINE liftLowerFT #-}

    runInterpretF i = runFreerFinalT $ FinalTInterpreter id i
    {-# INLINE runInterpretF #-}

    hoistFreer = hoistFreerFinal
    {-# INLINE hoistFreer #-}

    interpretFT f i = runFreerFinalT $ FinalTInterpreter f i

deriving via ViaLiftLower (FreerFinalT Monad) ins instance MonadTrans (FreerFinalT Monad ins)

instance MonadTransFreer (FreerFinalT Monad)

subsumeFreerFinal ::
    c (HeftiaFinalT c (LiftIns ins) f) =>
    FreerFinalT c ins (FreerFinalT c ins f) ~> FreerFinalT c ins f
subsumeFreerFinal =
    FreerFinalT
        . subsumeHeftiaFinal
        . hoistHeftiaFinal unFreerFinalT
        . unFreerFinalT
{-# INLINE subsumeFreerFinal #-}

dupFreerFinal :: FreerFinalT c ins f ~> FreerFinalT c ins (FreerFinalT c ins f)
dupFreerFinal = hoistFreerFinal liftLowerFTFinal
{-# INLINE dupFreerFinal #-}
