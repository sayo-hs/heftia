-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2023 Yamada Ryo
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable

A tree-structured encoded Heftia transformer.
-}
module Control.Monad.Trans.Heftia.Tree where

import Control.Effect.Class.Machinery.HFunctor (HFunctor, hfmap)
import Control.Monad (join, (<=<))
import Control.Monad.Cont (ContT (ContT), runContT)
import Control.Monad.Identity (Identity)
import Control.Monad.Trans (MonadTrans)
import Control.Monad.Trans.Free (FreeF (Free, Pure))
import Control.Monad.Trans.Heftia.Church (HeftiaChurchT (HeftiaChurchT), runHeftiaChurchT)
import Control.Monad.Trans.Hefty (HeftyT (HeftyT), liftHefty, runHeftyT)
import Data.Functor.Coyoneda (Coyoneda (Coyoneda))

newtype HCoyoneda h f a = HCoyoneda {unHCoyoneda :: Coyoneda (h f) a}
    deriving newtype (Functor, Applicative, Monad)

newtype HeftiaTreeT h m a = HeftiaTreeT {unHeftiaTreeT :: HeftyT (HCoyoneda h) m a}
    deriving newtype (Functor, Applicative, Monad, MonadTrans)

liftHeftiaTreeT :: Functor m => m a -> HeftiaTreeT h m a
liftHeftiaTreeT = HeftiaTreeT . liftHefty
{-# INLINE liftHeftiaTreeT #-}

toChurchHeftia :: (Monad m, HFunctor h) => HeftiaTreeT h m a -> HeftiaChurchT h m a
toChurchHeftia (HeftiaTreeT (HeftyT m)) =
    HeftiaChurchT \i -> ContT \k ->
        m >>= \case
            Pure x -> k x
            Free (HCoyoneda (Coyoneda f h)) ->
                runContT
                    (i $ hfmap toChurchHeftia' h)
                    ( (`runContT` k)
                        . runHeftiaChurchT i
                        . toChurchHeftia'
                        . f
                    )
              where
                toChurchHeftia' = toChurchHeftia . HeftiaTreeT

fromChurchHeftia :: (Monad m, HFunctor h) => HeftiaChurchT h m a -> HeftiaTreeT h m a
fromChurchHeftia (HeftiaChurchT f) =
    join . liftHeftiaTreeT $
        runContT
            ( f \h -> ContT \k -> do
                pure . HeftiaTreeT . HeftyT . pure . Free . HCoyoneda $
                    Coyoneda
                        (HeftyT . (runHeftyT . unHeftiaTreeT <=< k))
                        (hfmap (unHeftiaTreeT . fromChurchHeftia) h)
            )
            (pure . pure)

type HeftiaTree h = HeftiaTreeT h Identity
