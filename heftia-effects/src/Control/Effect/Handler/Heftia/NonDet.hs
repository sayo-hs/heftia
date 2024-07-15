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

import Control.Applicative (Alternative ((<|>)), empty, liftA2)
import Control.Effect.Hefty (Eff, injectF, interpretFin, interpretFinH_, interpretKH_)
import Control.Freer (Freer)
import Control.Monad.Freer (MonadFreer)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import Data.Effect.NonDet (Choose (Choose), Empty (Empty), LEmpty)
import Data.Functor.Compose (Compose (Compose), getCompose)
import Data.Hefty.Union (Union)

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

-- | 'Choose' effect handler for Applicative use.
runChooseA ::
    forall f ef a fr u c.
    ( Alternative f
    , Freer c fr
    , Union u
    , Applicative (Eff u fr '[] ef)
    , c (Compose (Eff u fr '[] ef) f)
    ) =>
    Eff u fr '[Choose] (LEmpty ': ef) a ->
    Eff u fr '[] ef (f a)
runChooseA =
    getCompose
        . interpretFinH_
            (Compose . runEmptyA . injectF)
            (\(Choose a b) -> Compose $ liftA2 (<|>) (runChooseA a) (runChooseA b))

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
