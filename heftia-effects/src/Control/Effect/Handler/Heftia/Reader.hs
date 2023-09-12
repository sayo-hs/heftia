-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Effect.Handler.Heftia.Reader where

import Control.Effect.Class (type (~>))
import Control.Effect.Class.Machinery.HFunctor (HFunctor)
import Control.Effect.Class.Reader (AskI (Ask), LocalS (Local), ask)
import Control.Effect.Freer (Fre, interpose, interpret, raise, type (<:))
import Control.Effect.Heftia (Hef, hoistHeftiaEffects, hoistInterpose, interpretH, raiseH)
import Data.Function ((&))
import Data.Hefty.Sum (SumH)

interpretReader ::
    (HFunctor (SumH es), Monad m) =>
    r ->
    Hef (LocalS r ': es) (Fre (AskI r ': es') m) ~> Hef es (Fre es' m)
interpretReader r = hoistHeftiaEffects (interpretAsk r) . interpretReaderH
{-# INLINE interpretReader #-}

interpretReaderH ::
    (AskI r <: es', HFunctor (SumH es), Monad m) =>
    Hef (LocalS r ': es) (Fre es' m) ~> Hef es (Fre es' m)
interpretReaderH =
    interpretH \(Local (f :: r -> r) a) ->
        a & hoistInterpose @(AskI r) \Ask -> f <$> ask

elaborateReader ::
    (AskI r <: es, Monad m) =>
    LocalS r (Fre es m) ~> Fre es m
elaborateReader (Local (f :: r -> r) a) =
    a & interpose @(AskI r) \Ask -> f <$> ask

interpretAsk :: Monad m => r -> Fre (AskI r ': es) m ~> Fre es m
interpretAsk r = interpret \Ask -> pure r
{-# INLINE interpretAsk #-}

liftReader ::
    (HFunctor (SumH es), Monad m) =>
    Hef es (Fre es' m) ~> Hef (LocalS FilePath ': es) (Fre (AskI FilePath ': es') m)
liftReader = raiseH . hoistHeftiaEffects raise
{-# INLINE liftReader #-}
