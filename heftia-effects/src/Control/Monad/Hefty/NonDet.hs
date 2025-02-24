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
import Control.Monad.Hefty (
    Eff,
    bundleN,
    interpret,
    interpretBy,
    interpretH,
    nil,
    (!+),
    (&),
    type (<<|),
    type (<|),
    type (~>),
 )
import Data.Bool (bool)
import Data.Effect.NonDet
import Data.Effect.Unlift (UnliftIO)
import UnliftIO (Exception, SomeException, throwIO, try)

-- | [NonDet]("Data.Effect.NonDet") effects handler for alternative answer type.
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
                !+ (\Empty _ -> pure A.empty)
                !+ nil
            )

-- | [NonDet]("Data.Effect.NonDet") effects handler for monoidal answer type.
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

-- | t'Choose' effect handler for alternative answer type.
runChoose
    :: forall f ef a
     . (Alternative f)
    => Eff '[] (Choose ': ef) a
    -> Eff '[] ef (f a)
runChoose =
    interpretBy (pure . pure) \Choose k ->
        liftA2 (<|>) (k False) (k True)

-- | t'Choose' effect handler for monoidal answer type.
runChooseMonoid
    :: forall ans ef a
     . (Semigroup ans)
    => (a -> Eff '[] ef ans)
    -> Eff '[] (Choose ': ef) a
    -> Eff '[] ef ans
runChooseMonoid f =
    interpretBy f \Choose k ->
        liftA2 (<>) (k False) (k True)

-- | t'Empty' effect handler.
runEmpty :: forall a ef. Eff '[] (Empty ': ef) a -> Eff '[] ef (Maybe a)
runEmpty =
    interpretBy
        (pure . Just)
        \Empty _ -> pure Nothing
