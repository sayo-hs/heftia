{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2023-2024 Yamada Ryo
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable

A Freer carrier that can be used as a handler for effect systems based
on [@classy-effects@](https://hackage.haskell.org/package/classy-effects).
-}
module Control.Effect.Free where

import Control.Applicative (Alternative)
import Control.Effect.Class (LiftIns (LiftIns), NopS, SendIns, sendIns, unliftIns, type (~>))
import Control.Effect.Class.Machinery.HFunctor (caseH, type (:+:) (Inr))
import Control.Effect.Hefty (
    DecompF,
    EffHeadF,
    EffTailF,
    Effectful (Effectful),
    MemberF,
    MultiToUnionF,
    SumToUnionF,
    unEffectful,
 )
import Control.Freer (Freer, InsClass, interpretFreer, liftIns, retractFreer, transformFreer)
import Control.Hefty (Hefty (Hefty), SigClass, unHefty)
import Control.Monad (MonadPlus)
import Control.Monad.Base (MonadBase)
import Control.Monad.Cont (Cont, ContT (ContT), runContT)
import Control.Monad.Freer (MonadFreer, interpretFreerK)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Identity (Identity (Identity))
import Control.Monad.Trans (MonadTrans (lift))
import Data.Coerce (coerce)
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

-- | A common wrapper data type for representing first-order extensible effectful programs.
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

-- | Interpret the leading first-order effect class.
interpretF ::
    forall er f u c.
    (Freer c f, Union u, DecompF u er) =>
    (MultiToUnionF u (EffHeadF er) ~> EffectfulF u f (EffTailF er)) ->
    EffectfulF u f er ~> EffectfulF u f (EffTailF er)
interpretF i =
    overEffectfulF $ interpretFreer \u ->
        case decomp u of
            Left e -> unEffectfulF $ i e
            Right e -> liftIns e

-- | Interpret the leading first-order effect class using a monad transformer.
interpretTF ::
    forall t er f u.
    (MonadFreer f, Union u, DecompF u er, MonadTrans t, Monad (t (EffectfulF u f (EffTailF er)))) =>
    (MultiToUnionF u (EffHeadF er) ~> t (EffectfulF u f (EffTailF er))) ->
    EffectfulF u f er ~> t (EffectfulF u f (EffTailF er))
interpretTF i = retractFreer . transformFreer (caseF i lift) . splitEffF @f
{-# INLINE interpretTF #-}

-- | Interpret the leading first-order effect class using a delimited continuation.
interpretKF ::
    forall er r a f u.
    (MonadFreer f, Union u, DecompF u er) =>
    (a -> EffectfulF u f (EffTailF er) r) ->
    ( forall x.
      (x -> EffectfulF u f (EffTailF er) r) ->
      MultiToUnionF u (EffHeadF er) x ->
      EffectfulF u f (EffTailF er) r
    ) ->
    EffectfulF u f er a ->
    EffectfulF u f (EffTailF er) r
interpretKF k i = (`runContT` k) . interpretContTF \e -> ContT (`i` e)
{-# INLINE interpretKF #-}

-- | Interpret the leading first-order effect class using a continuation monad transformer.
interpretContTF ::
    forall er r f u.
    (MonadFreer f, Union u, DecompF u er) =>
    (MultiToUnionF u (EffHeadF er) ~> ContT r (EffectfulF u f (EffTailF er))) ->
    EffectfulF u f er ~> ContT r (EffectfulF u f (EffTailF er))
interpretContTF i =
    transCont
        . interpretFreerK (detransContT . caseF i lift)
        . splitEffF @f
{-# INLINE interpretContTF #-}

transCont :: Cont (m r) ~> ContT r m
transCont (ContT f) = ContT \k -> coerce $ f $ coerce . k
{-# INLINE transCont #-}

detransContT :: ContT r m ~> Cont (m r)
detransContT (ContT f) = ContT \k -> coerce $ f $ coerce . k
{-# INLINE detransContT #-}

transformAllF ::
    forall u' e e' f u c.
    (Freer c f, Union u, Union u') =>
    (SumToUnionF u e ~> SumToUnionF u' e') ->
    EffectfulF u f e ~> EffectfulF u' f e'
transformAllF f = overEffectfulF $ transformFreer f
{-# INLINE transformAllF #-}

raiseF ::
    forall er f u c.
    (Freer c f, Union u, DecompF u er) =>
    EffectfulF u f (EffTailF er) ~> EffectfulF u f er
raiseF = transformAllF weaken
{-# INLINE raiseF #-}

raise2F ::
    forall e2 e1r f u c.
    (Freer c f, Union u, DecompF u e1r) =>
    EffectfulF u f (EffTailF e1r) ~> EffectfulF u f (e2 + e1r)
raise2F = transformAllF weaken2
{-# INLINE raise2F #-}

raise3F ::
    forall e3 e2 e1r f u c.
    (Freer c f, Union u, DecompF u e1r) =>
    EffectfulF u f (EffTailF e1r) ~> EffectfulF u f (e3 + e2 + e1r)
raise3F = transformAllF weaken3
{-# INLINE raise3F #-}

raise4F ::
    forall e4 e3 e2 e1r f u c.
    (Freer c f, Union u, DecompF u e1r) =>
    EffectfulF u f (EffTailF e1r) ~> EffectfulF u f (e4 + e3 + e2 + e1r)
raise4F = transformAllF weaken4
{-# INLINE raise4F #-}

raiseUnderF ::
    forall e1r e2 f u c.
    (Freer c f, Union u, DecompF u e1r) =>
    EffectfulF u f (e2 + EffTailF e1r) ~> EffectfulF u f (e2 + e1r)
raiseUnderF = transformAllF weakenUnder
{-# INLINE raiseUnderF #-}

flipEffF ::
    forall e1 e2 e f u c.
    (Freer c f, Union u) =>
    EffectfulF u f (e1 + e2 + e) ~> EffectfulF u f (e2 + e1 + e)
flipEffF = transformAllF flipUnion
{-# INLINE flipEffF #-}

splitEffF ::
    forall f' er f u c.
    (Freer c f', Freer c f, Union u, DecompF u er) =>
    EffectfulF u f er ~> f' (MultiToUnionF u (EffHeadF er) + EffectfulF u f (EffTailF er))
splitEffF (EffectfulF f) =
    f & interpretFreer \u -> case decomp u of
        Left e -> liftIns $ L1 e
        Right e -> liftIns $ R1 $ EffectfulF $ liftIns e

mergeEffF ::
    forall f' er f u c.
    (Freer c f', Freer c f, Union u, DecompF u er) =>
    f' (MultiToUnionF u (EffHeadF er) + EffectfulF u f (EffTailF er)) ~> EffectfulF u f er
mergeEffF =
    EffectfulF
        . interpretFreer
            ( caseF
                (liftIns . inject0)
                (transformFreer weaken . unEffectfulF)
            )

toEffectfulF :: forall e f u c. (Freer c f, Union u) => Effectful u f NopS e ~> EffectfulF u f e
toEffectfulF =
    EffectfulF
        . transformFreer (caseH absurdUnion unliftIns)
        . unHefty
        . unEffectful
{-# INLINE toEffectfulF #-}

fromEffectfulF :: forall e f u c. Freer c f => EffectfulF u f e ~> Effectful u f NopS e
fromEffectfulF =
    Effectful
        . Hefty
        . transformFreer (Inr . LiftIns)
        . unEffectfulF
{-# INLINE fromEffectfulF #-}
