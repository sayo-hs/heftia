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
module Control.Effect.Interpreter.Heftia.NonDet where

import Control.Applicative (Alternative ((<|>)), empty, liftA2, (<|>))
import Control.Arrow ((>>>))
import Control.Effect (type (~>))
import Control.Effect.Hefty (Eff, injectF, interpretFin, interpretFinH, interpretK, interpretRecH)
import Control.Freer (Freer)
import Control.Monad.Freer (MonadFreer)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import Data.Bool (bool)
import Data.Effect.HFunctor (HFunctor)
import Data.Effect.NonDet (Choose (Choose), ChooseH (ChooseH), Empty (Empty), LChoose, LEmpty, choose)
import Data.Functor.Compose (Compose (Compose), getCompose)
import Data.Hefty.Union (ForallHFunctor, HFunctorUnion, Member, Union)

-- | 'NonDet' effects handler for Monad use.
runNonDet ::
    forall f ef a fr u c.
    ( Alternative f
    , MonadFreer c fr
    , Union u
    , c (Eff u fr '[] ef)
    , c (Eff u fr '[] (LEmpty : ef))
    ) =>
    Eff u fr '[] (LChoose ': LEmpty ': ef) a ->
    Eff u fr '[] ef (f a)
runNonDet =
    runChoose >>> interpretK pure \_ Empty -> pure empty
{-# INLINE runNonDet #-}

-- | 'NonDet' effects handler for Monad use.
runNonDetK ::
    forall r ef a fr u c.
    ( Monoid r
    , MonadFreer c fr
    , Union u
    , c (Eff u fr '[] ef)
    , c (Eff u fr '[] (LEmpty ': ef))
    , HFunctor (u '[])
    ) =>
    (a -> Eff u fr '[] (LEmpty ': ef) r) ->
    Eff u fr '[] (LChoose ': LEmpty ': ef) a ->
    Eff u fr '[] ef r
runNonDetK f =
    runChooseK f >>> interpretK pure \_ Empty -> pure mempty
{-# INLINE runNonDetK #-}

-- | 'Choose' effect handler for Monad use.
runChoose ::
    forall f ef a fr u c.
    ( Alternative f
    , MonadFreer c fr
    , Union u
    , c (Eff u fr '[] ef)
    ) =>
    Eff u fr '[] (LChoose ': ef) a ->
    Eff u fr '[] ef (f a)
runChoose =
    interpretK (pure . pure) \k Choose ->
        liftA2 (<|>) (k False) (k True)

-- | 'Choose' effect handler for Monad use.
runChooseK ::
    forall r ef a fr u c.
    ( Semigroup r
    , MonadFreer c fr
    , Union u
    , c (Eff u fr '[] ef)
    ) =>
    (a -> Eff u fr '[] ef r) ->
    Eff u fr '[] (LChoose ': ef) a ->
    Eff u fr '[] ef r
runChooseK f =
    interpretK f \k Choose ->
        liftA2 (<>) (k False) (k True)

-- | 'Empty' effect handler for Monad use.
runEmpty ::
    forall a r fr u c.
    ( Freer c fr
    , Union u
    , Applicative (Eff u fr '[] r)
    , c (MaybeT (Eff u fr '[] r))
    ) =>
    Eff u fr '[] (LEmpty ': r) a ->
    Eff u fr '[] r (Maybe a)
runEmpty =
    runMaybeT . interpretFin
        (MaybeT . fmap Just . injectF)
        \Empty -> MaybeT $ pure Nothing

{- | 'ChooseH' effect handler for Monad use.

    Convert a higher-order effect of the form

        @chooseH :: m a -> m a -> m a@

    into a first-order effect of the form:

        @choose :: m Bool@
-}
runChooseH ::
    ( Freer c fr
    , HFunctorUnion u
    , Member u Choose ef
    , ForallHFunctor u eh
    , Monad (Eff u fr eh ef)
    ) =>
    Eff u fr (ChooseH ': eh) ef ~> Eff u fr eh ef
runChooseH =
    interpretRecH \(ChooseH a b) -> do
        world <- choose
        bool a b world

-- | 'NonDet' effect handler for Applicative use.
runNonDetA ::
    forall f ef a fr u c.
    ( Alternative f
    , Freer c fr
    , Union u
    , Applicative (Eff u fr '[] ef)
    , c (Compose (Eff u fr '[] ef) f)
    ) =>
    Eff u fr '[ChooseH] (LEmpty ': ef) a ->
    Eff u fr '[] ef (f a)
runNonDetA =
    getCompose
        . interpretFinH
            (Compose . runEmptyA . injectF)
            (\(ChooseH a b) -> Compose $ liftA2 (<|>) (runNonDetA a) (runNonDetA b))

-- | 'Empty' effect handler for Applicative use.
runEmptyA ::
    forall f a r fr u c.
    ( Alternative f
    , Freer c fr
    , Union u
    , Applicative (Eff u fr '[] r)
    , c (Compose (Eff u fr '[] r) f)
    ) =>
    Eff u fr '[] (LEmpty ': r) a ->
    Eff u fr '[] r (f a)
runEmptyA =
    getCompose
        . interpretFin
            (Compose . fmap pure . injectF)
            \Empty -> Compose $ pure empty
