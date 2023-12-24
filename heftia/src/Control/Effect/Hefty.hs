{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Effect.Hefty where

import Control.Applicative (Alternative)
import Control.Effect.Class (LiftIns, NopI, NopS, type (~>))
import Control.Effect.Class.Machinery.HFunctor (HFunctor, hfmap, (:+:))
import Control.Effect.Free (InsClass, MultiListToUnionF, MultiToUnionF, SumToUnionF)
import Control.Freer (Freer, interpretFreer, liftIns)
import Control.Hefty (Hefty (Hefty), SigClass, overHefty, unHefty)
import Control.Monad (MonadPlus)
import Control.Monad.Base (MonadBase)
import Control.Monad.IO.Class (MonadIO)
import Data.Free.Sum (caseSum, pattern L1, pattern R1, type (+))
import Data.Free.Union (Union, absurdUnion, decomp, inject0, weaken)
import Data.Kind (Type)

-- | A common monad wrapper data type for representing effectful programs.
newtype
    Effectful
        (u :: [InsClass] -> InsClass)
        (f :: InsClass -> Type -> Type)
        (eh :: SigClass)
        (ef :: InsClass)
        (a :: Type) = Effectful {unEffectful :: Hefty f (EffUnion u eh ef) a}

-- | Open union for higher-order effect classes and first-order effect classes.
newtype EffUnion u eh ef f a = EffUnion
    {unEffUnion :: (SumToUnionH u (NormalizeSig eh) f + SumToUnionF u ef) a}

-- | Manipulate the inside of the t'Effectful' wrapper.
overEffectful ::
    forall b eh' ef' f' u' a eh ef f u.
    (Hefty f (EffUnion u eh ef) a -> Hefty f' (EffUnion u' eh' ef') b) ->
    Effectful u f eh ef a ->
    Effectful u' f' eh' ef' b
overEffectful f = Effectful . f . unEffectful
{-# INLINE overEffectful #-}

instance TailHFunctor u eh => HFunctor (EffUnion u eh ef) where
    hfmap f =
        EffUnion
            . caseSum
                (L1 . unTailSigUnion . hfmap f . TailSigUnion @_ @(NormalizeSig eh))
                R1
            . unEffUnion
    {-# INLINE hfmap #-}

-- | Convert the sum of higher-order effects into an open union.
type SumToUnionH u e f = u (SumToUnionListH u e f)

{- |
Convert the sum of higher-order effects into an open union. If it's a single higher-order effect
rather than a sum, leave it as is without converting.
-}
type MultiToUnionH u e f = MultiListToUnionF u (SumToUnionListH u e f)

-- | t'HFunctor' constraint for effect class open unions.
type TailHFunctor u e = HFunctor (TailSigUnion u (NormalizeSig e))

-- | t'HFunctor' constraint for effect classes that are either single or in an open union.
type HeadHFunctor u e = HFunctor (HeadSigUnion u (NormalizeSig e))

{- |
Recursively decompose the sum of higher-order effects into a list, following the direction of right
association.
-}
type family
    SumToUnionListH
        (u :: [InsClass] -> InsClass)
        (e :: SigClass)
        (f :: Type -> Type) ::
        [InsClass]
    where
    SumToUnionListH u (LiftIns NopI) _ = '[]
    SumToUnionListH u (SingleSig e) f = '[e f]
    SumToUnionListH u (e1 :+: e2) f = MultiToUnionH u e1 f ': SumToUnionListH u e2 f

{- |
Normalization in preparation for decomposing the sum of effect classes into a list.

In particular, mark an indivisible, single effect class by applying the t'SingleSig' wrapper to it.
-}
type family NormalizeSig e where
    NormalizeSig (LiftIns NopI) = LiftIns NopI
    NormalizeSig (LiftIns (e1 + e2)) = NormalizeSig (LiftIns e1) :+: NormalizeSig (LiftIns e2)
    NormalizeSig (e1 :+: e2) = NormalizeSig e1 :+: NormalizeSig e2
    NormalizeSig e = SingleSig e

{- |
A wrapper to mark a single, i.e., a higher-order effect class that cannot be further decomposed as
a sum.
-}
newtype SingleSig (e :: SigClass) f a = SingleSig {unSingleSig :: e f a}

-- | A wrapper to provide an instance of t'HFunctor' for the open union.
newtype TailSigUnion u e f a = TailSigUnion
    {unTailSigUnion :: SumToUnionH u e f a}

-- | An 'hfmap' for higher-order effect class open unions.
hfmapTail ::
    forall u e f g.
    HFunctor (TailSigUnion u e) =>
    (f ~> g) ->
    SumToUnionH u e f ~> SumToUnionH u e g
hfmapTail f = unTailSigUnion . hfmap f . TailSigUnion @u @e
{-# INLINE hfmapTail #-}

instance Union u => HFunctor (TailSigUnion u (LiftIns NopI)) where
    hfmap _ = absurdUnion . unTailSigUnion
    {-# INLINE hfmap #-}

instance (HFunctor e, Union u) => HFunctor (TailSigUnion u (SingleSig e)) where
    hfmap f (TailSigUnion u) = TailSigUnion $ case decomp u of
        Left e -> inject0 $ hfmap f e
        Right e -> absurdUnion e
    {-# INLINE hfmap #-}

instance
    (HFunctor (HeadSigUnion u e1), HFunctor (TailSigUnion u e2), Union u) =>
    HFunctor (TailSigUnion u (e1 :+: e2))
    where
    hfmap f (TailSigUnion u) = TailSigUnion $ case decomp u of
        Left e -> inject0 $ hfmapHead @u @e1 f e
        Right e -> weaken $ hfmapTail @u @e2 f e
    {-# INLINE hfmap #-}

{- |
A data type where, if a specified higher-order effect class is a single entity that does not
decompose into a sum, it remains as is, but if it decomposes, an open union is applied.
-}
newtype HeadSigUnion u e f a = HeadSigUnion
    {unHeadSigUnion :: MultiToUnionH u e f a}

-- | An v'hfmap' for higher-order effect classes that are either single or in an open union.
hfmapHead ::
    forall u e f g.
    HFunctor (HeadSigUnion u e) =>
    (f ~> g) ->
    MultiToUnionH u e f ~> MultiToUnionH u e g
hfmapHead f = unHeadSigUnion . hfmap f . HeadSigUnion @u @e
{-# INLINE hfmapHead #-}

instance HFunctor (HeadSigUnion u (LiftIns NopI)) where
    hfmap _ = absurdNopI . unHeadSigUnion
    {-# INLINE hfmap #-}

-- | "Ex falso quodlibet" for the empty first-order effect class t'NopI'.
absurdNopI :: NopI a -> r
absurdNopI = \case {}
{-# INLINE absurdNopI #-}

instance HFunctor e => HFunctor (HeadSigUnion u (SingleSig e)) where
    hfmap f = HeadSigUnion . hfmap f . unHeadSigUnion
    {-# INLINE hfmap #-}

instance HFunctor (HeadSigUnion u e) => HFunctor (HeadSigUnion u (e :+: LiftIns NopI)) where
    hfmap f = HeadSigUnion . hfmapHead @u @e f . unHeadSigUnion
    {-# INLINE hfmap #-}

instance
    (HFunctor (HeadSigUnion u e1), HFunctor e2, Union u) =>
    HFunctor (HeadSigUnion u (e1 :+: SingleSig e2))
    where
    hfmap f (HeadSigUnion u) = HeadSigUnion case decomp u of
        Left e -> inject0 $ hfmapHead @u @e1 f e
        Right u' -> case decomp u' of
            Left e -> weaken $ inject0 $ hfmap f e
            Right e -> absurdUnion e

instance
    ( HFunctor (HeadSigUnion u e1)
    , HFunctor (HeadSigUnion u e2)
    , HFunctor (TailSigUnion u e3)
    , Union u
    ) =>
    HFunctor (HeadSigUnion u (e1 :+: (e2 :+: e3)))
    where
    hfmap f (HeadSigUnion u) = HeadSigUnion case decomp u of
        Left e -> inject0 $ hfmapHead @u @e1 f e
        Right u' -> weaken case decomp u' of
            Left e -> inject0 $ hfmapHead @u @e2 f e
            Right e -> weaken $ hfmapTail @u @e3 f e

deriving newtype instance Functor (Hefty f (EffUnion u eh ef)) => Functor (Effectful u f eh ef)
deriving newtype instance Applicative (Hefty f (EffUnion u eh ef)) => Applicative (Effectful u f eh ef)
deriving newtype instance Alternative (Hefty f (EffUnion u eh ef)) => Alternative (Effectful u f eh ef)
deriving newtype instance Monad (Hefty f (EffUnion u eh ef)) => Monad (Effectful u f eh ef)
deriving newtype instance MonadPlus (Hefty f (EffUnion u eh ef)) => MonadPlus (Effectful u f eh ef)
deriving newtype instance (MonadBase b (Hefty f (EffUnion u eh ef)), Monad b) => MonadBase b (Effectful u f eh ef)
deriving newtype instance MonadIO (Hefty f (EffUnion u eh ef)) => MonadIO (Effectful u f eh ef)
deriving newtype instance MonadFail (Hefty f (EffUnion u eh ef)) => MonadFail (Effectful u f eh ef)

deriving stock instance Foldable (Hefty f (EffUnion u eh ef)) => Foldable (Effectful u f eh ef)
deriving stock instance Traversable (Hefty f (EffUnion u eh ef)) => Traversable (Effectful u f eh ef)

deriving newtype instance Eq (Hefty f (EffUnion u eh ef) a) => Eq (Effectful u f eh ef a)
deriving newtype instance Ord (Hefty f (EffUnion u eh ef) a) => Ord (Effectful u f eh ef a)
deriving newtype instance Read (Hefty f (EffUnion u eh ef) a) => Read (Effectful u f eh ef a)
deriving newtype instance Show (Hefty f (EffUnion u eh ef) a) => Show (Effectful u f eh ef a)

{- |
Using the provided interpretation function, interpret first-order effects. For actions (scopes)
within higher-order effects that are currently unhandled, interpretation is applied recursively.

Note that if the interpretation function is stateful (i.e., not a monad morphism), the state is not
maintained across scopes.
-}
interpretRec ::
    forall e eh r f u c.
    (Freer c f, Union u, TailHFunctor u eh) =>
    (MultiToUnionF u e ~> Effectful u f eh r) ->
    Effectful u f eh (e + r) ~> Effectful u f eh r
interpretRec i =
    overEffectful
        . overHefty
        $ interpretFreer
        $ caseSum
            ( liftIns
                . EffUnion
                . L1
                . hfmapTail @u @(NormalizeSig eh) (unEffectful . interpretRec @e i . Effectful)
            )
            ( \u -> case decomp u of
                Left e -> unHefty $ unEffectful $ i e
                Right e -> liftIns $ EffUnion $ R1 e
            )
            . unEffUnion

{- |
Using the provided interpretation function, interpret higher-order effects. For actions (scopes)
within higher-order effects that are currently unhandled, interpretation is applied recursively.

Note that if the interpretation function is stateful (i.e., not a monad morphism), the state is not
maintained across scopes.
-}
interpretRecH ::
    forall e r ef f u c.
    (Freer c f, Union u, HeadHFunctor u e, TailHFunctor u r) =>
    (MultiToUnionH u (NormalizeSig e) (Hefty f (EffUnion u r ef)) ~> Effectful u f r ef) ->
    Effectful u f (e :+: r) ef ~> Effectful u f r ef
interpretRecH i =
    overEffectful
        . overHefty
        $ interpretFreer
        $ caseSum
            ( \u -> case decomp u of
                Left e -> unHefty $ unEffectful $ i $ hfmapHead @u @(NormalizeSig e) int e
                Right e -> liftIns $ EffUnion $ L1 $ hfmapTail @u @(NormalizeSig r) int e
            )
            (liftIns . EffUnion . R1)
            . unEffUnion
  where
    int :: Hefty f (EffUnion u (e :+: r) ef) ~> Hefty f (EffUnion u r ef)
    int = unEffectful . interpretRecH @e i . Effectful
    {-# INLINE int #-}

-- | Using the provided interpretation function, interpret first-order effects.
interpret ::
    forall e r f u c.
    (Freer c f, Union u) =>
    (MultiToUnionF u e ~> Effectful u f NopS r) ->
    Effectful u f NopS (e + r) ~> Effectful u f NopS r
interpret i =
    overEffectful
        . overHefty
        $ interpretFreer
        $ caseSum
            absurdUnion
            ( \u -> case decomp u of
                Left e -> unHefty $ unEffectful $ i e
                Right e -> liftIns $ EffUnion $ R1 e
            )
            . unEffUnion
