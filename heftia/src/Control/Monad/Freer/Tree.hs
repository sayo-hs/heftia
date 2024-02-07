{-# LANGUAGE DerivingVia #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2023 Yamada Ryo
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable

A tree-structured encoded Freer monad.
-}
module Control.Monad.Freer.Tree where

import Control.Applicative (Alternative)
import Control.Effect (type (~>))
import Control.Freer (Freer, interpretFreer, liftIns, transformFreer)
import Control.Monad (MonadPlus)
import Control.Monad.Cont (Cont, ContT (ContT), runCont)
import Control.Monad.Free (Free (Free, Pure), hoistFree, liftF)
import Control.Monad.Freer (MonadFreer, interpretFreerK)
import Control.Monad.Identity (Identity (Identity), runIdentity)
import Data.Functor.Coyoneda (Coyoneda (Coyoneda), hoistCoyoneda, liftCoyoneda)

-- | A tree-structured encoded Freer monad.
newtype FreerTree f a = FreerTree {unFreerTree :: Free (Coyoneda f) a}
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , Alternative
        , MonadPlus
        , Eq
        , Ord
        , Read
        , Show
        )
    deriving stock (Foldable, Traversable)

liftInsTree :: ins a -> FreerTree ins a
liftInsTree = FreerTree . liftF . liftCoyoneda
{-# INLINE liftInsTree #-}

interpretTree :: Monad m => (ins ~> m) -> FreerTree ins a -> m a
interpretTree i (FreerTree m) =
    case m of
        Pure x -> pure x
        Free (Coyoneda f e) -> i e >>= interpretTree i . FreerTree . f

interpretTreeK :: (e ~> Cont r) -> FreerTree e ~> Cont r
interpretTreeK i (FreerTree m) =
    case m of
        Pure x -> pure x
        Free (Coyoneda f e) ->
            ContT \k ->
                Identity $
                    runCont
                        (i e)
                        ((`runCont` runIdentity . k) . interpretTreeK i . FreerTree . f)

instance Freer Monad FreerTree where
    liftIns = liftInsTree
    interpretFreer = interpretTree
    transformFreer phi = FreerTree . hoistFree (hoistCoyoneda phi) . unFreerTree
    {-# INLINE liftIns #-}
    {-# INLINE interpretFreer #-}
    {-# INLINE transformFreer #-}

instance MonadFreer Monad FreerTree where
    interpretFreerK = interpretTreeK
    {-# INLINE interpretFreerK #-}
