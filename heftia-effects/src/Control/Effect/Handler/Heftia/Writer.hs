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

import Control.Effect.Class (type (~>))
import Control.Effect.Class.Writer (Tell (tell), TellI (Tell), WriterS (Censor, Listen))
import Control.Effect.Freer (Fre, intercept, interposeT, interpretK, interpretT, type (<|))
import Control.Monad.Trans.Writer.CPS (WriterT, runWriterT)
import Control.Monad.Trans.Writer.CPS qualified as T
import Data.Function ((&))
import Data.Tuple (swap)

elaborateWriterT ::
    forall w m es.
    (Monad m, Monoid w, TellI w <| es) =>
    WriterS w (Fre es m) ~> Fre es m
elaborateWriterT = \case
    Listen m -> listenT m
    Censor f m -> m & intercept @(TellI w) \(Tell w) -> Tell $ f w

elaborateWriterTransactionalT ::
    forall w m es.
    (Monad m, Monoid w, TellI w <| es) =>
    WriterS w (Fre es m) ~> Fre es m
elaborateWriterTransactionalT = \case
    Listen m -> listenT m
    Censor f m -> do
        (a, w) <- confiscateT m
        tell $ f w
        pure a

listenT ::
    (Monoid w, Monad m, TellI w <| es) =>
    Fre es m a ->
    Fre es m (a, w)
listenT m = do
    (a, w) <- confiscateT m
    tell w
    pure (a, w)
{-# INLINE listenT #-}

confiscateT ::
    forall w m es a.
    (Monoid w, Monad m, TellI w <| es) =>
    Fre es m a ->
    Fre es m (a, w)
confiscateT = runWriterT . interposeT @(TellI w) \(Tell w) -> T.tell w
{-# INLINE confiscateT #-}

interpretTell :: (Monad m, Monoid w) => Fre (TellI w ': es) m a -> Fre es m (w, a)
interpretTell = fmap swap . runWriterT . interpretTellT
{-# INLINE interpretTell #-}

interpretTellT :: (Monad m, Monoid w) => Fre (TellI w ': es) m a -> WriterT w (Fre es m) a
interpretTellT = interpretT \(Tell w) -> T.tell w
{-# INLINE interpretTellT #-}

interpretTellK :: (Monad m, Monoid w) => Fre (TellI w ': es) m a -> Fre es m (w, a)
interpretTellK =
    interpretK (pure . (mempty,)) \k (Tell w) -> do
        (w', r) <- k ()
        pure (w <> w', r)
