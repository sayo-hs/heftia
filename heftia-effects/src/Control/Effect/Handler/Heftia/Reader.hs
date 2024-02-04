-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Copyright   :  (c) 2023 Yamada Ryo
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable

Interpreter and elaborator for the t'Data.Effect.Reader.Local' / t'Data.Effect.Reader.Catch' effect
classes.
-}
module Control.Effect.Handler.Heftia.Reader where

import Control.Arrow ((>>>))
import Control.Effect (type (~>))
import Control.Effect.Hefty (
    Effectful,
    HFunctors,
    MemberF,
    TailHFunctor,
    interposeRec,
    interpretRec,
    interpretRecH,
 )
import Control.Freer (Freer)
import Data.Effect.HFunctor (type (:+:))
import Data.Effect.Reader (Ask (..), Local (..), ask)
import Data.Free.Sum (type (+))
import Data.Function ((&))
import Data.Hefty.Union (HFunctorUnion, Union)

interpretReader ::
    ( Freer c f
    , forall f'. c f' => Applicative f'
    , HFunctorUnion u
    , TailHFunctor u eh
    , Applicative (Effectful u f eh ef)
    , MemberF u (Ask r) (Ask r + ef)
    , c (Effectful u f eh ef)
    , c (Effectful u f eh (Ask r + ef))
    ) =>
    r ->
    Effectful u f (Local r :+: eh) (Ask r + ef) ~> Effectful u f eh ef
interpretReader r = interpretRecH elaborateLocal >>> interpretAsk r
{-# INLINE interpretReader #-}

-- | Elaborate the t'Local' effect.
elaborateLocal ::
    forall r eh ef f u c.
    ( MemberF u (Ask r) ef
    , Freer c f
    , Union u
    , HFunctors u eh
    , Functor (Effectful u f eh ef)
    ) =>
    Local r (Effectful u f eh ef) ~> Effectful u f eh ef
elaborateLocal (Local f a) = a & interposeRec @(Ask r) \Ask -> f <$> ask

-- | Interpret the t'Ask' effect.
interpretAsk ::
    forall r eh ef f u c.
    (Freer c f, Union u, Applicative (Effectful u f eh ef), HFunctors u eh) =>
    r ->
    Effectful u f eh (Ask r + ef) ~> Effectful u f eh ef
interpretAsk r = interpretRec \Ask -> pure r
{-# INLINE interpretAsk #-}
