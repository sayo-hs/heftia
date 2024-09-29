-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2023 Sayo Koyoneda
License     :  MPL-2.0 (see the LICENSE file)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable

Interpreter and elaborator for the t'Data.Effect.Reader.Local' / t'Data.Effect.Reader.Catch' effect
classes.
-}
module Control.Effect.Interpreter.Heftia.Reader where

import Control.Arrow ((>>>))
import Control.Effect (type (~>))
import Control.Effect.Hefty (
    Eff,
    Elab,
    interposeRec,
    interpretRec,
    interpretRecH,
 )
import Control.Freer (Freer)
import Data.Effect.HFunctor (HFunctor)
import Data.Effect.Reader (Ask (..), LAsk, Local (..), ask)
import Data.Function ((&))
import Data.Hefty.Union (ForallHFunctor, HFunctorUnion, Member, Union)

runReader
    :: forall r rh rf fr u c
     . ( Freer c fr
       , HFunctorUnion u
       , ForallHFunctor u rh
       , Member u (Ask r) (LAsk r ': rf)
       , Functor (Eff u fr rh (LAsk r ': rf))
       , Applicative (Eff u fr rh rf)
       )
    => r
    -> Eff u fr (Local r ': rh) (LAsk r ': rf) ~> Eff u fr rh rf
runReader r = runLocal >>> runAsk r
{-# INLINE runReader #-}

-- | Elaborate the t'Local' effect.
runLocal
    :: forall r rh ef fr u c
     . ( Freer c fr
       , HFunctorUnion u
       , ForallHFunctor u rh
       , Member u (Ask r) ef
       , Functor (Eff u fr rh ef)
       )
    => Eff u fr (Local r ': rh) ef ~> Eff u fr rh ef
runLocal = interpretRecH elabLocal
{-# INLINE runLocal #-}

elabLocal
    :: forall r eh ef fr u c
     . (Member u (Ask r) ef, Freer c fr, Union u, HFunctor (u eh), Functor (Eff u fr eh ef))
    => Elab (Local r) (Eff u fr eh ef)
elabLocal (Local f a) = a & interposeRec @(Ask r) \Ask -> f <$> ask

-- | Interpret the t'Ask' effect.
runAsk
    :: forall r rs eh fr u c
     . (Freer c fr, Union u, Applicative (Eff u fr eh rs), HFunctor (u eh))
    => r
    -> Eff u fr eh (LAsk r ': rs) ~> Eff u fr eh rs
runAsk r = interpretRec \Ask -> pure r
{-# INLINE runAsk #-}
