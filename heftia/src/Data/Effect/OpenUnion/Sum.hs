-- SPDX-License-Identifier: MPL-2.0

module Data.Effect.OpenUnion.Sum where

import Data.Effect (LNop, Nop)
import Data.Effect.HFunctor ((:+:))
import Data.Effect.OpenUnion.Internal.FO (Union)
import Data.Effect.OpenUnion.Internal.HO (UnionH)
import GHC.Generics qualified as G

infixr 5 +
type (+) = (G.:+:)

type U u e = SumToRecUnion u e
type UL u e = SumToRecUnionList u e

type SumToRecUnion u e = u (SumToRecUnionList u e)

type SumToRecUnionList :: forall k. ([k] -> k) -> k -> [k]
type family SumToRecUnionList u e where
    SumToRecUnionList Union Nop = '[]
    SumToRecUnionList Union (e + r) = e ': SumToRecUnionList Union r
    SumToRecUnionList Union e = '[e]
    SumToRecUnionList UnionH LNop = '[]
    SumToRecUnionList UnionH (e :+: r) = e ': SumToRecUnionList UnionH r
    SumToRecUnionList UnionH e = '[e]
