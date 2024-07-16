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
module Control.Effect.Handler.Heftia.NonDet where

import Control.Applicative (Alternative ((<|>)), asum, empty, liftA2, (<|>))
import Control.Arrow ((>>>))
import Control.Effect (type (~>))
import Control.Effect.Hefty (Eff, injectF, interpretFin, interpretFinH_, interpretK, interpretRecH)
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

-- | 'ChooseH' effect handler for Monad use.
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

{-
-- | 'NonDet' effects handler for Monad use.
runNonDet ::
    forall r ef a fr u c.
    ( Monoid r
    , MonadFreer c fr
    , Union u
    , c (Eff u fr '[] ef)
    , HFunctor (u '[Choose, LEmpty])
    ) =>
    (a -> Eff u fr '[] ef r) ->
    Eff u fr '[Choose, LEmpty] ef a ->
    Eff u fr '[] ef r
runNonDet k m = runContT (interpretContTAllH (elabNonDetThen end) m) k

-- | 'Choose' effect handler for Monad use.
runChoose ::
    forall r ef a fr u c.
    ( Semigroup r
    , MonadFreer c fr
    , Union u
    , c (Eff u fr '[] ef)
    ) =>
    (a -> Eff u fr '[] ef r) ->
    Eff u fr '[Choose] ef a ->
    Eff u fr '[] ef r
runChoose f =
    interpretKH_ f \k (Choose a b) ->
        liftA2 (<>) (runChoose k a) (runChoose k b)

elabChoose' ::
    forall r ef fr u c.
    ( Semigroup r
    , MonadFreer c fr
    , Union u
    , c (Eff u fr '[] ef)
    ) =>
    ElabK r Choose (Eff u fr '[] ef)
elabChoose' (Choose a b) =
    ContT \k -> liftA2 (<>) (runContT a k) (runContT b k)

elabChoose ::
    forall r ef fr u c.
    ( Semigroup r
    , MonadFreer c fr
    , Union u
    , c (Eff u fr '[] ef)
    ) =>
    ElabK r Choose (Eff u fr '[] ef)
elabChoose (Choose a b) =
    ContT \k -> liftA2 (<>) (runContT a k) (runContT b k)

elabEmpty ::
    forall r ef fr u c.
    ( MonadFreer c fr
    , Union u
    , c (Eff u fr '[] ef)
    ) =>
    r ->
    ElabK r LEmpty (Eff u fr '[] ef)
elabEmpty def LEmpty = ContT \_ -> pure def
{-# INLINE elabEmpty #-}

elabNonDetThen ::
    forall r eh ef fr u c.
    ( Monoid r
    , MonadFreer c fr
    , Union u
    , c (Eff u fr '[] ef)
    ) =>
    ElabK r (u eh) (Eff u fr '[] ef) ->
    ElabK r (u (Choose ': LEmpty ': eh)) (Eff u fr '[] ef)
elabNonDetThen elabTail = elabChoose |+: elabEmpty mempty |+: elabTail
{-# INLINE elabNonDetThen #-}
-}

-- | 'Choose' effect handler for Applicative use.
runChooseA ::
    forall f ef a fr u c.
    ( Alternative f
    , Freer c fr
    , Union u
    , Applicative (Eff u fr '[] ef)
    , c (Compose (Eff u fr '[] ef) f)
    ) =>
    Eff u fr '[ChooseH] (LEmpty ': ef) a ->
    Eff u fr '[] ef (f a)
runChooseA =
    getCompose
        . interpretFinH_
            (Compose . runEmptyA . injectF)
            (\(ChooseH a b) -> Compose $ liftA2 (<|>) (runChooseA a) (runChooseA b))

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
