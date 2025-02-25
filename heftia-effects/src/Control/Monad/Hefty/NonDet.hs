{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2024 Sayo contributors
License     :  MPL-2.0 (see the LICENSE file)
Maintainer  :  ymdfield@outlook.jp

Interpreters for the [non-determinism]("Data.Effect.NonDet") effects.
-}
module Control.Monad.Hefty.NonDet (
    module Control.Monad.Hefty.NonDet,
    module Data.Effect.NonDet,
)
where

import Control.Applicative (Alternative ((<|>)), (<|>))
#if ( __GLASGOW_HASKELL__ < 906 )
import Control.Applicative (liftA2)
#endif
import Control.Applicative qualified as A
import Control.Arrow ((>>>))
import Control.Monad.Hefty (Eff, FOEs, bundle, interpretBy, nil, (!+))
import Data.Effect.NonDet

-- | [NonDet]("Data.Effect.NonDet") effects handler for alternative answer type.
runNonDet
    :: forall f es a
     . (Alternative f, FOEs es)
    => Eff (Choose ': Empty ': es) a
    -> Eff es (f a)
runNonDet =
    bundle
        >>> interpretBy
            (pure . pure)
            ( (\Choose k -> liftA2 (<|>) (k False) (k True))
                !+ (\Empty _ -> pure A.empty)
                !+ nil
            )
{-# INLINE runNonDet #-}

-- | [NonDet]("Data.Effect.NonDet") effects handler for monoidal answer type.
runNonDetMonoid
    :: forall ans es a
     . (Monoid ans, FOEs es)
    => (a -> Eff es ans)
    -> Eff (Choose ': Empty ': es) a
    -> Eff es ans
runNonDetMonoid f =
    bundle
        >>> interpretBy
            f
            ( (\Choose k -> liftA2 (<>) (k False) (k True))
                !+ (\Empty _ -> pure mempty)
                !+ nil
            )
{-# INLINE runNonDetMonoid #-}

-- | t'Choose' effect handler for alternative answer type.
runChoose
    :: forall f es a
     . (Alternative f, FOEs es)
    => Eff (Choose ': es) a
    -> Eff es (f a)
runChoose =
    interpretBy (pure . pure) \Choose k ->
        liftA2 (<|>) (k False) (k True)
{-# INLINE runChoose #-}

-- | t'Choose' effect handler for monoidal answer type.
runChooseMonoid
    :: forall ans es a
     . (Semigroup ans, FOEs es)
    => (a -> Eff es ans)
    -> Eff (Choose ': es) a
    -> Eff es ans
runChooseMonoid f =
    interpretBy f \Choose k ->
        liftA2 (<>) (k False) (k True)
{-# INLINE runChooseMonoid #-}

-- | t'Empty' effect handler.
runEmpty :: forall a es. (FOEs es) => Eff (Empty ': es) a -> Eff es (Maybe a)
runEmpty =
    interpretBy
        (pure . Just)
        \Empty _ -> pure Nothing
{-# INLINE runEmpty #-}
