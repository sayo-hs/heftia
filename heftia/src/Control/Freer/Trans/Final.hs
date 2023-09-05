{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Freer.Trans.Final where

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
    liftLowerHFinal,
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

liftLowerFinal :: f ~> FreerFinalT c ins f
liftLowerFinal = FreerFinalT . liftLowerHFinal
{-# INLINE liftLowerFinal #-}

weakenFreerFinalT :: (forall g. c' g => c g) => FreerFinalT c ins f ~> FreerFinalT c' ins f
weakenFreerFinalT = FreerFinalT . weakenHeftiaFinalT . unFreerFinalT
{-# INLINE weakenFreerFinalT #-}

hoistFreerFinal :: (f ~> g) -> FreerFinalT c ins f ~> FreerFinalT c ins g
hoistFreerFinal phi = FreerFinalT . hoistHeftiaFinal phi . unFreerFinalT
{-# INLINE hoistFreerFinal #-}

deriving newtype instance
    (forall g. c g => Functor g, c (HeftiaFinalT c (LiftIns ins) f)) =>
    Functor (FreerFinalT c ins f)

deriving newtype instance
    ( forall g. c g => Applicative g
    , c (HeftiaFinalT c (LiftIns ins) f)
    , c (FreerFinalT c ins f)
    ) =>
    Applicative (FreerFinalT c ins f)

deriving newtype instance
    ( forall g. c g => Alternative g
    , c (HeftiaFinalT c (LiftIns ins) f)
    , c (FreerFinalT c ins f)
    ) =>
    Alternative (FreerFinalT c ins f)

deriving newtype instance
    ( forall g. c g => Monad g
    , c (HeftiaFinalT c (LiftIns ins) f)
    , c (FreerFinalT c ins f)
    ) =>
    Monad (FreerFinalT c ins f)

deriving newtype instance
    ( forall g. c g => MonadPlus g
    , c (HeftiaFinalT c (LiftIns ins) f)
    , c (FreerFinalT c ins f)
    ) =>
    MonadPlus (FreerFinalT c ins f)

instance (forall h f. c f => c (FreerFinalT c h f)) => TransFreer c (FreerFinalT c) where
    liftInsT = liftInsFinalT
    {-# INLINE liftInsT #-}

    liftLower = liftLowerFinal
    {-# INLINE liftLower #-}

    runInterpretF i = runFreerFinalT $ FinalTInterpreter id i
    {-# INLINE runInterpretF #-}

    hoistFreer = hoistFreerFinal
    {-# INLINE hoistFreer #-}

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
dupFreerFinal = hoistFreerFinal liftLowerFinal
{-# INLINE dupFreerFinal #-}