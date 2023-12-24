{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Effect.Free where

import Control.Applicative (Alternative)
import Control.Effect.Class (NopI, type (~>))
import Control.Freer (Freer, interpretFreer, liftIns, retractFreer, transformFreer)
import Control.Monad (MonadPlus)
import Control.Monad.Base (MonadBase)
import Control.Monad.Freer (MonadFreer)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans (MonadTrans (lift))
import Data.Free.Sum (caseSum, pattern L1, pattern R1, type (+))
import Data.Free.Union (Union, decomp, flipUnion, inject0, weaken, weaken2, weaken3, weaken4, weakenUnder)
import Data.Function ((&))
import Data.Kind (Type)

-- | A common monad wrapper data type for representing first-order effectful programs.
newtype
    EffectfulF
        (u :: [InsClass] -> InsClass)
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

-- | Convert the sum of first-order effects into an open union.
type SumToUnionF u e = u (SumToUnionListF u e)

{- |
Convert the sum of first-order effects into an open union. If it's a single first-order effect
rather than a sum, leave it as is without converting.
-}
type MultiToUnionF u e = MultiListToUnionF u (SumToUnionListF u e)

{- |
Convert a given list of first-order effect classes into a suitable representation type for each case
of being empty, single, or multiple.
-}
type family MultiListToUnionF (u :: [InsClass] -> InsClass) (es :: [InsClass]) :: InsClass where
    MultiListToUnionF u '[] = NopI
    MultiListToUnionF u '[e] = e
    MultiListToUnionF u es = u es

{- |
Recursively decompose the sum of first-order effects into a list, following the direction of right
association.
-}
type family SumToUnionListF (u :: [InsClass] -> InsClass) (e :: InsClass) :: [InsClass] where
    SumToUnionListF u (e1 + e2) = MultiToUnionF u e1 ': SumToUnionListF u e2
    SumToUnionListF u NopI = '[]
    SumToUnionListF u e = '[e]

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

type InsClass = Type -> Type

-- Using the provided interpretation function, interpret first-order effects.
interpretF ::
    forall e r f u c.
    (Freer c f, Union u) =>
    (MultiToUnionF u e ~> EffectfulF u f r) ->
    EffectfulF u f (e + r) ~> EffectfulF u f r
interpretF i =
    overEffectfulF $ interpretFreer \u ->
        case decomp @u u of
            Left e -> unEffectfulF $ i e
            Right e -> liftIns e

interpretTF ::
    forall t e r f u.
    (MonadFreer f, Union u, MonadTrans t, Monad (t (EffectfulF u f r))) =>
    (MultiToUnionF u e ~> t (EffectfulF u f r)) ->
    EffectfulF u f (e + r) ~> t (EffectfulF u f r)
interpretTF i = retractFreer . transformFreer (caseSum i lift) . splitF @f
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

splitF ::
    forall f' e r f u c.
    (Freer c f', Freer c f, Union u) =>
    EffectfulF u f (e + r) ~> f' (MultiToUnionF u e + EffectfulF u f r)
splitF (EffectfulF f) =
    f & interpretFreer \u -> case decomp u of
        Left e -> liftIns $ L1 e
        Right e -> liftIns $ R1 $ EffectfulF $ liftIns e

mergeF ::
    forall f' e r f u c.
    (Freer c f', Freer c f, Union u) =>
    f' (MultiToUnionF u e + EffectfulF u f r) ~> EffectfulF u f (e + r)
mergeF =
    EffectfulF
        . interpretFreer
            ( caseSum
                (liftIns . inject0)
                (transformFreer weaken . unEffectfulF)
            )
