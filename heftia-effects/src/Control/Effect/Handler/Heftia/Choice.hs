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
module Control.Effect.Handler.Heftia.Choice where

import Control.Applicative (liftA2)
import Control.Effect.Hefty (Eff, interpretK)
import Control.Monad.Freer (MonadFreer)
import Data.Effect.Choice (Choice (Choice), LChoice)
import Data.Hefty.Union (Union)

runChoice ::
    forall r ef a fr u c.
    ( Semigroup r
    , MonadFreer c fr
    , Union u
    , c (Eff u fr '[] ef)
    ) =>
    (a -> Eff u fr '[] ef r) ->
    Eff u fr '[] (LChoice ': ef) a ->
    Eff u fr '[] ef r
runChoice f =
    interpretK f \k Choice ->
        liftA2 (<>) (k False) (k True)
