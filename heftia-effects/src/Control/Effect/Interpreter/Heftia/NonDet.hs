-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2024 Sayo Koyoneda
License     :  MPL-2.0 (see the LICENSE file)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable
-}
module Control.Effect.Interpreter.Heftia.NonDet where

import Control.Applicative (Alternative ((<|>)), empty, (<|>))
import Control.Arrow ((>>>))
import Control.Effect (type (~>))
import Control.Monad.Hefty
import Data.Bool (bool)
import Data.Effect.NonDet (Choose (Choose), ChooseH (ChooseH), Empty (Empty), choose)

-- | 'NonDet' effects handler for alternative answer type.
runNonDet
    :: forall f r a
     . (Alternative f)
    => Eff '[] (Choose ': Empty ': r) a
    -> Eff '[] r (f a)
runNonDet =
    runChoose
        >>> interpretBy pure \Empty _ -> pure empty

-- | 'NonDet' effects handler for monoidal answer type.
runNonDetMonoid
    :: forall ans r a
     . (Monoid ans)
    => (a -> Eff '[] (Empty ': r) ans)
    -> Eff '[] (Choose ': Empty ': r) a
    -> Eff '[] r ans
runNonDetMonoid f =
    runChooseMonoid f
        >>> interpretBy pure \Empty _ -> pure mempty

-- | 'Choose' effect handler for alternative answer type.
runChoose
    :: forall f ef a
     . (Alternative f)
    => Eff '[] (Choose ': ef) a
    -> Eff '[] ef (f a)
runChoose =
    interpretBy (pure . pure) \Choose k ->
        liftA2 (<|>) (k False) (k True)

-- | 'Choose' effect handler for monoidal answer type.
runChooseMonoid
    :: forall ans r a
     . (Semigroup ans)
    => (a -> Eff '[] r ans)
    -> Eff '[] (Choose ': r) a
    -> Eff '[] r ans
runChooseMonoid f =
    interpretBy f \Choose k ->
        liftA2 (<>) (k False) (k True)

-- | 'Empty' effect handler.
runEmpty :: forall a r. Eff '[] (Empty ': r) a -> Eff '[] r (Maybe a)
runEmpty =
    interpretBy
        (pure . Just)
        \Empty _ -> pure Nothing

{- | 'ChooseH' effect elaborator.

    Convert a higher-order effect of the form

        @chooseH :: m a -> m a -> m a@

    into a first-order effect of the form:

        @choose :: m Bool@
-}
runChooseH :: (Choose <| ef) => Eff (ChooseH ': eh) ef ~> Eff eh ef
runChooseH =
    interpretRecH \(ChooseH a b) -> do
        world <- choose
        bool a b world
