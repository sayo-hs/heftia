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

import Control.Effect (type (~>))

import Control.Effect.Hefty (Eff, EffUnion (EffUnion), MemberF, SumToUnionListNF, caseHF)
import Control.Freer (Freer, InjectIns, ViaFreer (ViaFreer), injectIns, transformFreer, viaFreer)
import Control.Hefty (Hefty (Hefty), unHefty)
import Data.Effect (LiftIns (LiftIns), Nop, SigClass)
import Data.Free.Sum (pattern R1)
import Data.Hefty.Union (Union, exhaust, injectRec)

{- |
A common type for representing first-order extensible effectful programs that can issue effects
belonging to the specified sum of effect classes.
-}
type EffectfulF u fr e = EffF u fr (SumToUnionListNF u e)

{- |
A common type for representing first-order extensible effectful programs that can issue effects
belonging to the specified list of effect classes.
-}
type EffF u fr es = ViaFreer fr (EffUnionF u es)

-- | A common wrapper data type for representing first-order extensible effect union.
newtype EffUnionF (u :: [SigClass] -> SigClass) es a = EffUnionF {unEffUnionF :: u es Nop a}

instance MemberF u e es => InjectIns e (EffUnionF u es) where
    injectIns = EffUnionF . injectRec . LiftIns
    {-# INLINE injectIns #-}

toEffF :: forall es fr u c. (Freer c fr, Union u) => Eff u fr '[] es ~> EffF u fr es
toEffF =
    ViaFreer
        . transformFreer (caseHF exhaust EffUnionF)
        . unHefty
{-# INLINE toEffF #-}

fromEffectfulF :: forall es fr u c. Freer c fr => EffF u fr es ~> Eff u fr '[] es
fromEffectfulF =
    Hefty
        . transformFreer (EffUnion . R1 . unEffUnionF)
        . viaFreer
{-# INLINE fromEffectfulF #-}

{-  all types of interpret-family functions:
        - interpret   :                 e  ~> E r           ->    E (e + r)  ~> E r
        - reinterpret :                 e1 ~> E (e2 + r)    ->    E (e1 + r) ~> E (e2 + r)
        - intercept   :  e <| es  =>    e  ~> E es          ->    E es       ~> E es

        all possible suffix patterns of interpret-family functions:
            - <none>
            - K
            - ContT
            - T

    all types of transform-family functions:
        - transform :                  e1 ~> e2    ->    E (e1 + r) ~> E (e2 + r)
        - translate :  e2 <| r   =>    e1 ~> e2    ->    E (e1 + r) ~> E r
        - rewrite   :  e  <| es  =>    e  ~> e     ->    E es       ~> E es

    todo patterns: all ( 4x3 + 3 = 16 functions )
-}
