-- from: https://hackage.haskell.org/package/freer-simple-1.2.1.2/docs/src/Data.OpenUnion.Internal.html#Union
-- BSD3, (c) 2016 Allele Dev; 2017 Ixperta Solutions s.r.o.; 2017 Alexis King

module Data.Hefty.OpenUnion where

import Control.Effect (type (~>))
import Control.Effect.Free qualified as E
import Control.Effect.Hefty qualified as E
import Data.Effect (SigClass)
import Data.Effect.HFunctor (HFunctor, hfmap)
import Data.Hefty.Union (
    ClassIndex,
    HFunctorUnion_,
    Union (
        HasMembership,
        exhaust,
        inject,
        inject0,
        project,
        weaken,
        (|+:)
    ),
 )
import Data.Hefty.Union qualified as U
import Data.Hefty.Union qualified as Union
import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import GHC.TypeLits (natVal)
import GHC.TypeNats (KnownNat)
import Unsafe.Coerce (unsafeCoerce)

data OpenUnion (es :: [SigClass]) (f :: Type -> Type) (a :: Type) where
    OpenUnion :: {-# UNPACK #-} !Word -> t a -> OpenUnion es f a

unsafeInj :: Word -> t a -> OpenUnion es f a
unsafeInj = OpenUnion
{-# INLINE unsafeInj #-}

unsafePrj :: Word -> OpenUnion es f a -> Maybe (t a)
unsafePrj n (OpenUnion n' x)
    | n == n' = Just (unsafeCoerce x)
    | otherwise = Nothing
{-# INLINE unsafePrj #-}

instance Union OpenUnion where
    type HasMembership _ e es = KnownNat (ClassIndex es e)

    inject :: forall e es f. HasMembership OpenUnion e es => e f ~> OpenUnion es f
    inject = unsafeInj $ fromInteger $ natVal @(ClassIndex es e) Proxy
    {-# INLINE inject #-}

    project :: forall e es f a. HasMembership OpenUnion e es => OpenUnion es f a -> Maybe (e f a)
    project = unsafePrj $ fromInteger $ natVal @(ClassIndex es e) Proxy
    {-# INLINE project #-}

    exhaust = error "unreachable"
    {-# INLINE exhaust #-}

    inject0 = unsafeInj 0
    {-# INLINE inject0 #-}

    weaken (OpenUnion n a) = OpenUnion (n + 1) a
    {-# INLINE weaken #-}

    (f |+: g) (OpenUnion n a) = case n of
        0 -> f $ unsafeCoerce a
        _ -> g $ OpenUnion (n - 1) a
    {-# INLINE (|+:) #-}

instance ForallHFunctor es => HFunctor (OpenUnion es) where
    hfmap f (OpenUnion n a) = OpenUnion n $ f $ unsafeCoerce a
    {-# INLINE hfmap #-}

class ForallHFunctor es
instance (HFunctor e, ForallHFunctor r) => ForallHFunctor (e ': r)
instance ForallHFunctor '[]

instance HFunctorUnion_ ForallHFunctor OpenUnion where
    type ForallHFunctor _ = ForallHFunctor

type e <| es = U.Member OpenUnion e es
type e <<| es = U.MemberH OpenUnion e es

type MemberBy key e efs = U.MemberBy OpenUnion key e efs
type MemberHBy key e ehs = U.MemberHBy OpenUnion key e ehs

infix 3 <|
infix 3 <<|

type U ef = Union.U OpenUnion ef
type UH eh = Union.UH OpenUnion eh

type S ef = Union.S OpenUnion ef
type SH eh = Union.SH OpenUnion eh

type Eff fr eh ef = E.Eff OpenUnion fr eh ef
type EffF fr es = E.EffF OpenUnion fr es
