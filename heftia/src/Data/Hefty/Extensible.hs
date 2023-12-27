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

An implementation of an open union for higher-order effects using
the [extensible](https://hackage.haskell.org/package/extensible) package as a backend.
-}
module Data.Hefty.Extensible where

import Control.Effect.Class.Machinery.HFunctor (HFunctor, hfmap)
import Control.Hefty (SigClass)
import Data.Extensible (Forall, Match (Match), htabulateFor, match)
import Data.Extensible.Sum (exhaust, strikeAt, (<:|), type (:/) (EmbedAt))
import Data.Hefty.Union (
    HFunctorUnion_ (ForallHFunctor),
    Union (
        HasMembership,
        absurdUnion,
        inject,
        inject0,
        project,
        weaken,
        (|+:)
    ),
 )
import Data.Proxy (Proxy (Proxy))
import Data.Type.Equality ((:~:) (Refl))
import GHC.TypeLits (ErrorMessage (ShowType, Text, (:<>:)), TypeError)
import GHC.TypeNats (KnownNat, Nat, type (+))
import Type.Membership.Internal (
    Elaborate,
    Elaborated (Expecting),
    FindType,
    Member (membership),
    Membership,
    leadership,
    nextMembership,
 )
import Unsafe.Coerce (unsafeCoerce)

{- |
An implementation of an open union for higher-order effects using
the [extensible](https://hackage.haskell.org/package/extensible) package as a backend.
-}
newtype ExtensibleUnion es f a = ExtensibleUnion {unExtensibleUnion :: es :/ FieldApp f a}

newtype FieldApp f a (e :: SigClass) = FieldApp {unFieldApp :: e f a}

instance Forall HFunctor es => HFunctor (ExtensibleUnion es) where
    hfmap f =
        ExtensibleUnion
            . match
                ( htabulateFor @HFunctor Proxy \w ->
                    Match $ EmbedAt w . FieldApp . hfmap f . unFieldApp
                )
            . unExtensibleUnion
    {-# INLINE hfmap #-}

-- todo: Functor, Foldable, Traversable instances

instance Union ExtensibleUnion where
    type HasMembership _ e es = KnownNat (TypeIndex es e)

    inject = ExtensibleUnion . EmbedAt findFirstMembership . FieldApp
    {-# INLINE inject #-}

    project (ExtensibleUnion u) = unFieldApp <$> strikeAt findFirstMembership u
    {-# INLINE project #-}

    absurdUnion = exhaust . unExtensibleUnion
    {-# INLINE absurdUnion #-}

    inject0 = ExtensibleUnion . EmbedAt leadership . FieldApp
    {-# INLINE inject0 #-}

    weaken (ExtensibleUnion (EmbedAt w e)) =
        ExtensibleUnion $ EmbedAt (nextMembership w) e
    {-# INLINE weaken #-}

    f |+: g = (f . unFieldApp <:| g . ExtensibleUnion) . unExtensibleUnion
    {-# INLINE (|+:) #-}

findFirstMembership :: forall xs x. KnownNat (TypeIndex xs x) => Membership xs x
findFirstMembership = unsafeMkMembership @(TypeIndex xs x) Proxy
  where
    -- This hack may break if the membership package version gets updated.
    unsafeMkMembership :: forall pos. Proxy pos -> KnownNat pos => Membership xs x
    unsafeMkMembership _ = case hackedEquality of Refl -> membership
      where
        hackedEquality :: Elaborate x (FindType x xs) :~: 'Expecting pos
        hackedEquality = unsafeCoerce Refl

type family TypeIndex (xs :: [k]) (x :: k) :: Nat where
    TypeIndex (x ': xs) x = 0
    TypeIndex (y ': xs) x = 1 + TypeIndex xs x
    TypeIndex '[] x =
        TypeError
            ('Text "The effect class " ':<>: 'ShowType x ':<>: 'Text " was not found in the list.")

instance HFunctorUnion_ (Forall HFunctor) ExtensibleUnion where
    type ForallHFunctor _ = Forall HFunctor
