{-# LANGUAGE CPP #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2024 Sayo Koyoneda
License     :  MPL-2.0 (see the LICENSE file)
Maintainer  :  ymdfield@outlook.jp
Portability :  portable
-}
module Control.Monad.Hefty.NonDet where

import Control.Applicative (Alternative ((<|>)), empty, (<|>))
#if ( __GLASGOW_HASKELL__ < 906 )
import Control.Applicative (liftA2)
#endif
import Control.Arrow ((>>>))
import Control.Monad.Hefty (
    Eff,
    bundleN,
    interpretBy,
    interpretH,
    nil,
    (!+),
    type (<|),
    type (~>),
 )
import Data.Bool (bool)
import Data.Effect.NonDet (Choose (Choose), ChooseH (ChooseH), Empty (Empty), choose)

-- | 'NonDet' effects handler for alternative answer type.
runNonDet
    :: forall f ef a
     . (Alternative f)
    => Eff '[] (Choose ': Empty ': ef) a
    -> Eff '[] ef (f a)
runNonDet =
    bundleN @2
        >>> interpretBy
            (pure . pure)
            ( (\Choose k -> liftA2 (<|>) (k False) (k True))
                !+ (\Empty _ -> pure empty)
                !+ nil
            )

-- | 'NonDet' effects handler for monoidal answer type.
runNonDetMonoid
    :: forall ans ef a
     . (Monoid ans)
    => (a -> Eff '[] ef ans)
    -> Eff '[] (Choose ': Empty ': ef) a
    -> Eff '[] ef ans
runNonDetMonoid f =
    bundleN @2
        >>> interpretBy
            f
            ( (\Choose k -> liftA2 (<>) (k False) (k True))
                !+ (\Empty _ -> pure mempty)
                !+ nil
            )

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
    :: forall ans ef a
     . (Semigroup ans)
    => (a -> Eff '[] ef ans)
    -> Eff '[] (Choose ': ef) a
    -> Eff '[] ef ans
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
runChooseH
    :: (Choose <| ef)
    => Eff (ChooseH ': eh) ef ~> Eff eh ef
runChooseH = interpretH \(ChooseH a b) -> branch a b

-- | Faster than `<|>`.
branch :: (Choose <| ef) => Eff eh ef a -> Eff eh ef a -> Eff eh ef a
branch a b = do
    world <- choose
    bool a b world
{-# INLINE branch #-}
