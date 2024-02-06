{-# LANGUAGE AllowAmbiguousTypes #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2023 Yamada Ryo
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable

Interpreter and elaborator for the t'Control.Effect.Class.Writer.Writer' effect class.
See [README.md](https://github.com/sayo-hs/heftia/blob/master/README.md).
-}
module Control.Effect.Handler.Heftia.Writer where

import Control.Effect (type (~>))
import Control.Effect.Hefty (Eff, Elab, MemberF, interposeT, interpretK, interpretT, rewrite)
import Control.Monad.Freer (MonadFreer)
import Control.Monad.Trans.Writer.CPS (WriterT, runWriterT)
import Control.Monad.Trans.Writer.CPS qualified as T
import Data.Effect.HFunctor (HFunctor)
import Data.Effect.Writer (LTell, Tell (Tell), WriterH (Censor, Listen), tell)
import Data.Function ((&))
import Data.Hefty.Union (Union)
import Data.Tuple (swap)

elaborateWriter ::
    forall w ef fr u.
    (Monoid w, MonadFreer fr, Union u, MemberF u (Tell w) ef, HFunctor (u '[])) =>
    Elab (WriterH w) (Eff u fr '[] ef)
elaborateWriter = \case
    Listen m -> listenT m
    Censor f m -> m & rewrite @(Tell w) \(Tell w) -> Tell $ f w

elaborateWriterTransactional ::
    forall w ef fr u.
    (Monoid w, MonadFreer fr, Union u, MemberF u (Tell w) ef) =>
    Elab (WriterH w) (Eff u fr '[] ef)
elaborateWriterTransactional = \case
    Listen m -> listenT m
    Censor f m -> do
        (a, w) <- confiscateT m
        tell $ f w
        pure a

listenT ::
    forall w es a fr u.
    (Monoid w, MonadFreer fr, Union u, MemberF u (Tell w) es) =>
    Eff u fr '[] es a ->
    Eff u fr '[] es (a, w)
listenT m = do
    (a, w) <- confiscateT m
    tell w
    pure (a, w)
{-# INLINE listenT #-}

confiscateT ::
    forall w es a fr u.
    (Monoid w, MonadFreer fr, Union u, MemberF u (Tell w) es) =>
    Eff u fr '[] es a ->
    Eff u fr '[] es (a, w)
confiscateT = runWriterT . interposeT @(Tell w) \(Tell w) -> T.tell w
{-# INLINE confiscateT #-}

interpretTell :: (Monoid w, MonadFreer fr, Union u) => Eff u fr '[] (LTell w ': r) a -> Eff u fr '[] r (w, a)
interpretTell = fmap swap . runWriterT . interpretTellT
{-# INLINE interpretTell #-}

interpretTellT :: (Monoid w, MonadFreer fr, Union u) => Eff u fr '[] (LTell w ': r) ~> WriterT w (Eff u fr '[] r)
interpretTellT = interpretT \(Tell w) -> T.tell w
{-# INLINE interpretTellT #-}

interpretTellK :: (Monoid w, MonadFreer fr, Union u) => Eff u fr '[] (LTell w ': r) a -> Eff u fr '[] r (w, a)
interpretTellK =
    interpretK (pure . (mempty,)) \k (Tell w) -> do
        (w', r) <- k ()
        pure (w <> w', r)
