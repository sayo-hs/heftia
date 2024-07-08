-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2024 Yamada Ryo
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable
-}
module Control.Effect.Handler.Heftia.Output where

import Control.Arrow ((>>>))
import Control.Effect (type (~>))
import Control.Effect.Handler.Heftia.State (interpretState)
import Control.Effect.Handler.Heftia.Writer (interpretTell, interpretTell')
import Control.Effect.Hefty (Eff, interpret, interpretRec, raiseUnder, send0)
import Control.Freer (Freer)
import Control.Monad.Trans.State (StateT)
import Control.Monad.Trans.Writer.CPS qualified as CPS
import Control.Monad.Trans.Writer.Strict qualified as Strict
import Data.Effect.HFunctor (HFunctor)
import Data.Effect.Output (LOutput, Output (Output))
import Data.Effect.State (LState, State, modify)
import Data.Effect.Writer (Tell (Tell))
import Data.Hefty.Union (Member, Union)

runOutputEff ::
    (Freer c fr, Union u, HFunctor (u eh)) =>
    (o -> Eff u fr eh r ()) ->
    Eff u fr eh (LOutput o ': r) ~> Eff u fr eh r
runOutputEff f = interpretRec \(Output o) -> f o
{-# INLINE runOutputEff #-}

ignoreOutput ::
    (Freer c fr, Union u, HFunctor (u eh), Applicative (Eff u fr eh r)) =>
    Eff u fr eh (LOutput o ': r) ~> Eff u fr eh r
ignoreOutput = runOutputEff $ const $ pure ()
{-# INLINE ignoreOutput #-}

runOutputList ::
    forall o a r fr u c.
    ( Freer c fr
    , Union u
    , c (Eff u fr '[] r)
    , c (StateT [o] (Eff u fr '[] r))
    , Applicative (Eff u fr '[] r)
    , Monad (Eff u fr '[] (LState [o] ': r))
    , Member u (State [o]) (LState [o] ': r)
    , HFunctor (u '[])
    ) =>
    Eff u fr '[] (LOutput o ': r) a ->
    Eff u fr '[] r ([o], a)
runOutputList =
    raiseUnder
        >>> interpret (\(Output o) -> modify (o :))
        >>> interpretState []

{- | Run an `Output` effect by transforming into a monoid.
     The carrier is required to be a monad.
-}
runOutputMonoid ::
    forall o m a r fr u c.
    ( Monoid m
    , Freer c fr
    , Union u
    , Monad (Eff u fr '[] r)
    , c (CPS.WriterT m (Eff u fr '[] r))
    , HFunctor (u '[])
    ) =>
    (o -> m) ->
    Eff u fr '[] (LOutput o ': r) a ->
    Eff u fr '[] r (m, a)
runOutputMonoid f =
    raiseUnder
        >>> interpret (\(Output o) -> send0 $ Tell $ f o)
        >>> interpretTell

{- | Strict version of `runOutputMonoid`.
     The constraint on the carrier has been weakened to applicative.
-}
runOutputMonoid' ::
    forall o m a r fr u c.
    ( Monoid m
    , Freer c fr
    , Union u
    , Applicative (Eff u fr '[] r)
    , c (Strict.WriterT m (Eff u fr '[] r))
    , HFunctor (u '[])
    ) =>
    (o -> m) ->
    Eff u fr '[] (LOutput o ': r) a ->
    Eff u fr '[] r (m, a)
runOutputMonoid' f =
    raiseUnder
        >>> interpret (\(Output o) -> send0 $ Tell $ f o)
        >>> interpretTell'
