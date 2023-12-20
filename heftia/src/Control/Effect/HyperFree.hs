{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Effect.HyperFree where

import Control.Applicative (Alternative)
import Control.Effect.Class (type (~>))
import Control.Effect.Free (InsClass, ManyToUnion, SumToUnion)
import Control.Freer (Freer, interpretFreer, liftIns)
import Control.HyperFree (
    ASigClass (ASigClass),
    GetSigClass,
    HyperFree,
    LiftIns,
    SigClass,
    hyfmap,
    overHyperFree,
    unHyperFree,
    (:#),
    type (#),
 )
import Control.Monad (MonadPlus)
import Control.Monad.Base (MonadBase)
import Control.Monad.IO.Class (MonadIO)
import Data.Coerce (coerce)
import Data.Free.Sum (caseSum, pattern L1, pattern R1, type (+))
import Data.Free.Union (Union, absurdUnion, decomp, inject0, weaken)
import Data.HyperFree.Sum (SFunctor, sfmap, type (+#), type (:+:))
import Data.Kind (Type)

-- | A common monad wrapper data type for representing effectful programs.
newtype
    Effectful
        (u :: [InsClass] -> InsClass)
        (f :: InsClass -> Type -> Type)
        (eh :: ASigClass)
        (ef :: InsClass)
        (a :: Type) = Effectful {unEffectful :: HyperFree f ('ASigClass (EffUnion u eh ef)) a}

newtype EffUnion u eh ef h e a = EffUnion
    {unEffUnion :: (SigUnion eh u h e + SumToUnion u ef) a}

instance SFunctors u eh => SFunctor (EffUnion u eh ef) where
    sfmap f = EffUnion . caseSum (L1 . sfmap f) R1 . unEffUnion
    {-# INLINE sfmap #-}

newtype SigUnionForSFunctor isSingle e u h e' a = SigUnionForSFunctor
    {unSigUnionForSFunctor :: SumToUnion u (UnliftSig e h e') a}

sigUnion ::
    forall e u h e' a.
    SumToUnion u (UnliftSig e h e') a ->
    SigUnion e u h e' a
sigUnion = coerce
{-# INLINE sigUnion #-}

unSigUnion ::
    forall e u h e' a.
    SigUnion e u h e' a ->
    SumToUnion u (UnliftSig e h e') a
unSigUnion = coerce
{-# INLINE unSigUnion #-}

type SigUnion e = SigUnionForSFunctor (IsSingleSig (GetSigClass e)) e

type family IsSingleSig e where
    IsSingleSig (_ :+: _) = 'False
    IsSingleSig (LiftIns _) = 'False
    IsSingleSig _ = 'True

type UnliftSig e h e' = UnliftSig_ (IsSingleSig (GetSigClass e)) (GetSigClass e) h e'

type family
    UnliftSig_
        (isSingle :: Bool)
        (e :: SigClass)
        (h :: ASigClass -> Type -> Type)
        (e' :: ASigClass) ::
        InsClass
    where
    UnliftSig_ 'True e h e' = e h e'
    UnliftSig_ 'False (e1 :+: e2) h e' =
        e1 h e' + UnliftSig ('ASigClass e2) h e'
    UnliftSig_ 'False (LiftIns e) _ _ = e

type SFunctors u e = SFunctor (SigUnion e u)

instance
    (SFunctor e1, SFunctors u ('ASigClass e2), Union u) =>
    SFunctor (SigUnionForSFunctor 'False ('ASigClass (e1 :+: e2)) u)
    where
    sfmap f (SigUnionForSFunctor u) =
        SigUnionForSFunctor $ case decomp u of
            Left e -> inject0 $ sfmap f e
            Right e ->
                weaken
                    . unSigUnion @('ASigClass e2)
                    . sfmap f
                    $ sigUnion e

instance SFunctor (SigUnionForSFunctor 'False ('ASigClass (LiftIns e)) u) where
    sfmap _ = sigUnion . unSigUnion
    {-# INLINE sfmap #-}

instance
    (SFunctor e, IsSingleSig e ~ 'True, Union u) =>
    SFunctor (SigUnionForSFunctor 'True ('ASigClass e) u)
    where
    sfmap f =
        sigUnion
            . ( \u -> case decomp u of
                    Left e -> inject0 $ sfmap f e
                    Right e -> absurdUnion e
              )
            . unSigUnion
    {-# INLINE sfmap #-}

deriving newtype instance Functor (f :# EffUnion u eh ef) => Functor (Effectful u f eh ef)
deriving newtype instance Applicative (f :# EffUnion u eh ef) => Applicative (Effectful u f eh ef)
deriving newtype instance Alternative (f :# EffUnion u eh ef) => Alternative (Effectful u f eh ef)
deriving newtype instance Monad (f :# EffUnion u eh ef) => Monad (Effectful u f eh ef)
deriving newtype instance MonadPlus (f :# EffUnion u eh ef) => MonadPlus (Effectful u f eh ef)
deriving newtype instance (MonadBase b (f :# EffUnion u eh ef), Monad b) => MonadBase b (Effectful u f eh ef)
deriving newtype instance MonadIO (f :# EffUnion u eh ef) => MonadIO (Effectful u f eh ef)
deriving newtype instance MonadFail (f :# EffUnion u eh ef) => MonadFail (Effectful u f eh ef)

deriving stock instance Foldable (f :# EffUnion u eh ef) => Foldable (Effectful u f eh ef)
deriving stock instance Traversable (f :# EffUnion u eh ef) => Traversable (Effectful u f eh ef)

deriving newtype instance Eq ((f :# EffUnion u eh ef) a) => Eq (Effectful u f eh ef a)
deriving newtype instance Ord ((f :# EffUnion u eh ef) a) => Ord (Effectful u f eh ef a)
deriving newtype instance Read ((f :# EffUnion u eh ef) a) => Read (Effectful u f eh ef a)
deriving newtype instance Show ((f :# EffUnion u eh ef) a) => Show (Effectful u f eh ef a)

interpret ::
    forall e eh r f u c.
    (Freer c f, Union u, SFunctors u eh) =>
    (ManyToUnion u e ~> Effectful u f eh r) ->
    Effectful u f eh (e + r) ~> Effectful u f eh r
interpret i =
    overEffectful
        . overHyperFree
        $ interpretFreer
        $ caseSum
            (liftIns . EffUnion . L1 . sfmap (unEffectful . interpret i . Effectful))
            ( \u -> case decomp u of
                Left e -> unHyperFree $ unEffectful $ i e
                Right e -> liftIns $ EffUnion $ R1 e
            )
            . unEffUnion

{-
-- todo: modify precedence of '~>'
interpretH ::
    forall e r ef f u c.
    (Freer c f, Union u, SFunctor e, SFunctors u r) =>
    ((e (HyperFree f) # EffUnion u r ef) ~> Effectful u f r ef) ->
    Effectful u f (e +# r) ef ~> Effectful u f r ef
interpretH i =
    overEffectful
        . overHyperFree
        $ interpretFreer
        $ caseSum
            ( ( \u -> case decomp u of
                    Left e ->
                        unHyperFree
                            . unEffectful
                            . i
                            $ sfmap (unEffectful . interpretH i . Effectful) e
                    Right e ->
                        liftIns
                            . EffUnion
                            . L1
                            . sfmap (unEffectful . interpretH i . Effectful)
                            $ sigUnion e
              )
                . unSigUnion
            )
            (liftIns . EffUnion . R1)
            . unEffUnion
-}

overEffectful ::
    forall b eh' ef' f' u' a eh ef f u.
    (HyperFree f ('ASigClass (EffUnion u eh ef)) a -> HyperFree f' ('ASigClass (EffUnion u' eh' ef')) b) ->
    Effectful u f eh ef a ->
    Effectful u' f' eh' ef' b
overEffectful f = Effectful . f . unEffectful
{-# INLINE overEffectful #-}
