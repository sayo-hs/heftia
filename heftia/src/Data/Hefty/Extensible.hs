{-# LANGUAGE UndecidableInstances #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Data.Hefty.Extensible where

import Control.Effect.Class (Signature)
import Control.Effect.Class.Machinery.HFunctor (HFunctor, hfmap)
import Data.Extensible (Forall, Match (Match), htabulateFor, leadership, match)
import Data.Extensible.Sum (exhaust, strikeAt, (<:|), type (:/) (EmbedAt))
import Data.Free.Extensible (TypeIndex, findFirstMembership)
import Data.Hefty.Union (
    UnionH (
        HasMembershipH,
        absurdUnionH,
        inject0H,
        injectH,
        projectH,
        weakenH,
        (|+:)
    ),
 )
import Data.Proxy (Proxy (Proxy))
import GHC.TypeNats (KnownNat)
import Type.Membership (nextMembership)

newtype ExtensibleUnionH hs f a = ExtensibleUnionH {unExtensibleUnionH :: hs :/ FieldAppH f a}

newtype FieldAppH f a (h :: Signature) = FieldAppH {unFieldAppH :: h f a}

instance Forall HFunctor hs => HFunctor (ExtensibleUnionH hs) where
    hfmap f =
        ExtensibleUnionH
            . match
                ( htabulateFor @HFunctor Proxy \w ->
                    Match $ EmbedAt w . FieldAppH . hfmap f . unFieldAppH
                )
            . unExtensibleUnionH
    {-# INLINE hfmap #-}

-- todo: Functor, Foldable, Traversable instances

instance UnionH ExtensibleUnionH where
    type HasMembershipH _ h hs = KnownNat (TypeIndex hs h)

    injectH = ExtensibleUnionH . EmbedAt findFirstMembership . FieldAppH
    {-# INLINE injectH #-}

    projectH (ExtensibleUnionH u) = unFieldAppH <$> strikeAt findFirstMembership u
    {-# INLINE projectH #-}

    absurdUnionH = exhaust . unExtensibleUnionH
    {-# INLINE absurdUnionH #-}

    inject0H = ExtensibleUnionH . EmbedAt leadership . FieldAppH
    {-# INLINE inject0H #-}

    weakenH (ExtensibleUnionH (EmbedAt w e)) =
        ExtensibleUnionH $ EmbedAt (nextMembership w) e
    {-# INLINE weakenH #-}

    f |+: g = (f . unFieldAppH <:| g . ExtensibleUnionH) . unExtensibleUnionH
    {-# INLINE (|+:) #-}
