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

An implementation of an open union for first-order effects using
the [extensible](https://hackage.haskell.org/package/extensible) package as a backend.
-}
module Data.Free.Extensible where

import Control.Effect.Class (Instruction)
import Data.Extensible (Forall, Match (Match), htabulateFor, leadership, match)
import Data.Extensible.Sum (exhaust, strikeAt, (<:|), type (:/) (EmbedAt))
import Data.Free.Union (
    Union (
        HasMembership,
        absurdUnion,
        inject,
        inject0,
        project,
        weaken,
        (|+|:)
    ),
 )
import Data.Proxy (Proxy (Proxy))
import GHC.TypeLits (ErrorMessage (ShowType, Text, (:<>:)), TypeError)
import GHC.TypeNats (KnownNat, Nat, natVal, type (+))
import Type.Membership (Membership, nextMembership)
import Unsafe.Coerce (unsafeCoerce)

{- |
An implementation of an open union for first-order effects using
the [extensible](https://hackage.haskell.org/package/extensible) package as a backend.
-}
newtype ExtensibleUnion fs a = ExtensibleUnion {unExtensibleUnion :: fs :/ FieldApp a}

newtype FieldApp a (f :: Instruction) = FieldApp {unFieldApp :: f a}

instance Forall Functor fs => Functor (ExtensibleUnion fs) where
    fmap f =
        ExtensibleUnion
            . match
                ( htabulateFor @Functor Proxy \w ->
                    Match \e -> EmbedAt w $ FieldApp $ f <$> unFieldApp e
                )
            . unExtensibleUnion
    {-# INLINE fmap #-}

{- todo:
instance Forall Foldable fs => Foldable (ExtensibleUnion fs) where
instance Forall Traversable fs => Traversable (ExtensibleUnion fs) where
-}

instance Union ExtensibleUnion where
    type HasMembership _ f fs = KnownNat (TypeIndex fs f)

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

    f |+|: g = (f . unFieldApp <:| g . ExtensibleUnion) . unExtensibleUnion
    {-# INLINE (|+|:) #-}

findFirstMembership :: forall xs x. KnownNat (TypeIndex xs x) => Membership xs x
findFirstMembership = unsafeMkMembership $ fromIntegral $ natVal @(TypeIndex xs x) Proxy
  where
    -- This hack may break if the membership package version gets updated.
    unsafeMkMembership :: Int -> Membership xs x
    unsafeMkMembership = unsafeCoerce

type family TypeIndex (xs :: [k]) (x :: k) :: Nat where
    TypeIndex (x ': xs) x = 0
    TypeIndex (y ': xs) x = 1 + TypeIndex xs x
    TypeIndex '[] x =
        TypeError
            ('Text "The effect class " ':<>: 'ShowType x ':<>: 'Text " was not found in the list.")
