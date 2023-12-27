{-# LANGUAGE UndecidableInstances #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2023 Yamada Ryo
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable

A Freer carrier that can be used as a handler for effect systems based
on [@classy-effects@](https://hackage.haskell.org/package/classy-effects).
-}
module Control.Effect.Free where

import Control.Applicative (Alternative)
import Control.Effect.Class (LiftIns (LiftIns), NopS, SendIns, sendIns, type (~>))
import Control.Effect.Hefty (
    EffUnion (EffUnion),
    Effectful (Effectful),
    MemberF,
    MultiToUnionF,
    SumToUnionF,
    unEffUnion,
    unEffUnionH,
    unEffectful,
 )
import Control.Freer (Freer, InsClass, interpretFreer, liftIns, retractFreer, transformFreer)
import Control.Hefty (Hefty (Hefty), SigClass, unHefty)
import Control.Monad (MonadPlus)
import Control.Monad.Base (MonadBase)
import Control.Monad.Freer (MonadFreer)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans (MonadTrans (lift))
import Data.Free.Sum (caseF, pattern L1, pattern R1, type (+))
import Data.Function ((&))
import Data.Hefty.Union (
    Union,
    absurdUnion,
    decomp,
    flipUnion,
    inject0,
    injectRec,
    weaken,
    weaken2,
    weaken3,
    weaken4,
    weakenUnder,
 )
import Data.Kind (Type)

-- | A common wrapper data type for representing first-order effectful programs.
newtype
    EffectfulF
        (u :: [SigClass] -> SigClass)
        (f :: InsClass -> Type -> Type)
        (e :: InsClass)
        (a :: Type) = EffectfulF {unEffectfulF :: f (SumToUnionF u e) a}

-- | Manipulate the inside of the t'EffectfulF' wrapper.
overEffectfulF ::
    forall b e' f' u' a e f u.
    (f (SumToUnionF u e) a -> f' (SumToUnionF u' e') b) ->
    EffectfulF u f e a ->
    EffectfulF u' f' e' b
overEffectfulF f = EffectfulF . f . unEffectfulF
{-# INLINE overEffectfulF #-}

deriving newtype instance Functor (f (SumToUnionF u e)) => Functor (EffectfulF u f e)
deriving newtype instance Applicative (f (SumToUnionF u e)) => Applicative (EffectfulF u f e)
deriving newtype instance Alternative (f (SumToUnionF u e)) => Alternative (EffectfulF u f e)
deriving newtype instance Monad (f (SumToUnionF u e)) => Monad (EffectfulF u f e)
deriving newtype instance MonadPlus (f (SumToUnionF u e)) => MonadPlus (EffectfulF u f e)
deriving newtype instance (MonadBase b (f (SumToUnionF u e)), Monad b) => MonadBase b (EffectfulF u f e)
deriving newtype instance MonadIO (f (SumToUnionF u e)) => MonadIO (EffectfulF u f e)
deriving newtype instance MonadFail (f (SumToUnionF u e)) => MonadFail (EffectfulF u f e)

deriving stock instance Foldable (f (SumToUnionF u e)) => Foldable (EffectfulF u f e)
deriving stock instance Traversable (f (SumToUnionF u e)) => Traversable (EffectfulF u f e)

deriving newtype instance Eq (f (SumToUnionF u e) a) => Eq (EffectfulF u f e a)
deriving newtype instance Ord (f (SumToUnionF u e) a) => Ord (EffectfulF u f e a)
deriving newtype instance Read (f (SumToUnionF u e) a) => Read (EffectfulF u f e a)
deriving newtype instance Show (f (SumToUnionF u e) a) => Show (EffectfulF u f e a)

instance (MemberF u e es, Freer c fr) => SendIns e (EffectfulF u fr es) where
    sendIns = EffectfulF . liftIns . injectRec . LiftIns
    {-# INLINE sendIns #-}

-- Using the provided interpretation function, interpret first-order effects.
interpretF ::
    forall e r f u c.
    (Freer c f, Union u) =>
    (MultiToUnionF u e ~> EffectfulF u f r) ->
    EffectfulF u f (e + r) ~> EffectfulF u f r
interpretF i =
    overEffectfulF $ interpretFreer \u ->
        case decomp u of
            Left e -> unEffectfulF $ i e
            Right e -> liftIns e

interpretTF ::
    forall t e r f u.
    (MonadFreer f, Union u, MonadTrans t, Monad (t (EffectfulF u f r))) =>
    (MultiToUnionF u e ~> t (EffectfulF u f r)) ->
    EffectfulF u f (e + r) ~> t (EffectfulF u f r)
interpretTF i = retractFreer . transformFreer (caseF i lift) . splitEffF @f
{-# INLINE interpretTF #-}

transformAllF ::
    forall u' e e' f u c.
    (Freer c f, Union u, Union u') =>
    (SumToUnionF u e ~> SumToUnionF u' e') ->
    EffectfulF u f e ~> EffectfulF u' f e'
transformAllF f = overEffectfulF $ transformFreer f
{-# INLINE transformAllF #-}

raiseF ::
    forall e1 e f u c.
    (Freer c f, Union u) =>
    EffectfulF u f e ~> EffectfulF u f (e1 + e)
raiseF = transformAllF weaken
{-# INLINE raiseF #-}

raise2F ::
    forall e1 e2 e f u c.
    (Freer c f, Union u) =>
    EffectfulF u f e ~> EffectfulF u f (e1 + e2 + e)
raise2F = transformAllF weaken2
{-# INLINE raise2F #-}

raise3F ::
    forall e1 e2 e3 e f u c.
    (Freer c f, Union u) =>
    EffectfulF u f e ~> EffectfulF u f (e1 + e2 + e3 + e)
raise3F = transformAllF weaken3
{-# INLINE raise3F #-}

raise4F ::
    forall e1 e2 e3 e4 e f u c.
    (Freer c f, Union u) =>
    EffectfulF u f e ~> EffectfulF u f (e1 + e2 + e3 + e4 + e)
raise4F = transformAllF weaken4
{-# INLINE raise4F #-}

raiseUnderF ::
    forall e1 e2 e f u c.
    (Freer c f, Union u) =>
    EffectfulF u f (e1 + e) ~> EffectfulF u f (e1 + e2 + e)
raiseUnderF = transformAllF weakenUnder
{-# INLINE raiseUnderF #-}

flipEffectfulF ::
    forall e1 e2 e f u c.
    (Freer c f, Union u) =>
    EffectfulF u f (e1 + e2 + e) ~> EffectfulF u f (e2 + e1 + e)
flipEffectfulF = transformAllF flipUnion
{-# INLINE flipEffectfulF #-}

splitEffF ::
    forall f' e r f u c.
    (Freer c f', Freer c f, Union u) =>
    EffectfulF u f (e + r) ~> f' (MultiToUnionF u e + EffectfulF u f r)
splitEffF (EffectfulF f) =
    f & interpretFreer \u -> case decomp u of
        Left e -> liftIns $ L1 e
        Right e -> liftIns $ R1 $ EffectfulF $ liftIns e

mergeEffF ::
    forall f' e r f u c.
    (Freer c f', Freer c f, Union u) =>
    f' (MultiToUnionF u e + EffectfulF u f r) ~> EffectfulF u f (e + r)
mergeEffF =
    EffectfulF
        . interpretFreer
            ( caseF
                (liftIns . inject0)
                (transformFreer weaken . unEffectfulF)
            )

fromEffectful :: forall e f u c. (Freer c f, Union u) => Effectful u f NopS e ~> EffectfulF u f e
fromEffectful =
    EffectfulF
        . transformFreer (caseF (absurdUnion . unEffUnionH) id . unEffUnion)
        . unHefty
        . unEffectful
{-# INLINE fromEffectful #-}

toEffectful :: forall e f u c. Freer c f => EffectfulF u f e ~> Effectful u f NopS e
toEffectful =
    Effectful
        . Hefty
        . transformFreer (EffUnion . R1)
        . unEffectfulF
{-# INLINE toEffectful #-}
