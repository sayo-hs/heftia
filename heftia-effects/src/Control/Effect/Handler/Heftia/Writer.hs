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
import Control.Effect.Hefty (Eff, Elab, interposeT, interpretK, interpretT, rewrite, interpretFin, interposeFin, injectF)
import Control.Monad.Freer (MonadFreer)
import Control.Monad.Trans.Writer.CPS qualified as CPS
import Control.Monad.Trans.Writer.Strict qualified as Strict
import Data.Effect.HFunctor (HFunctor)
import Data.Effect.Writer (LTell, Tell (Tell), WriterH (Censor, Listen), tell)
import Data.Function ((&))
import Data.Hefty.Union (Union, Member)
import Data.Tuple (swap)
import Control.Monad.Trans (lift)
import Control.Freer (Freer)

elaborateWriterPost ::
    forall w ef fr u c.
    ( Monoid w
    , Freer c fr
    , Union u
    , Member u (Tell w) ef
    , HFunctor (u '[])
    , Monad (Eff u fr '[] ef)
    , c (CPS.WriterT w (Eff u fr '[] ef))
    ) =>
    Elab (WriterH w) (Eff u fr '[] ef)
elaborateWriterPost = \case
    Listen m -> listenT m
    Censor f m -> postCensor f m

postCensor ::
    forall w es fr u c.
    ( Monoid w
    , Freer c fr
    , Member u (Tell w) es
    , Union u
    , HFunctor (u '[])
    , Monad (Eff u fr '[] es)
    , c (CPS.WriterT w (Eff u fr '[] es))
    ) =>
    (w -> w) -> Eff u fr '[] es ~> Eff u fr '[] es
postCensor f m = do
    (a, w) <- CPS.runWriterT $ confiscateT m
    tell $ f w
    pure a


elaborateWriterPre ::
    forall w ef fr u c.
    ( Monoid w
    , Freer c fr
    , Union u
    , Member u (Tell w) ef
    , HFunctor (u '[])
    , Monad (Eff u fr '[] ef)
    , c (CPS.WriterT w (Eff u fr '[] ef))
    ) =>
    Elab (WriterH w) (Eff u fr '[] ef)
elaborateWriterPre = \case
    Listen m -> listenT m
    Censor f m -> preCensor f m

elaborateWriterPre' ::
    forall w ef fr u c.
    ( Monoid w
    , Freer c fr
    , Union u
    , Member u (Tell w) ef
    , HFunctor (u '[])
    , Applicative (Eff u fr '[] ef)
    , c (Strict.WriterT w (Eff u fr '[] ef))
    ) =>
    Elab (WriterH w) (Eff u fr '[] ef)
elaborateWriterPre' = \case
    Listen m -> listenT' m
    Censor f m -> preCensor f m

preCensor ::
    forall w es fr u c. (Freer c fr, Member u (Tell w) es, Union u, HFunctor (u '[])) =>
    (w -> w) -> Eff u fr '[] es ~> Eff u fr '[] es
preCensor f = rewrite @(Tell w) \(Tell w) -> Tell $ f w


listenT ::
    forall w es a fr u c.
    ( Monoid w
    , Freer c fr
    , Union u
    , Member u (Tell w) es
    , Monad (Eff u fr '[] es)
    , c (CPS.WriterT w (Eff u fr '[] es))
    ) =>
    Eff u fr '[] es a ->
    Eff u fr '[] es (w, a)
listenT m =
    swap <$> CPS.runWriterT do
        m & interposeT @(Tell w) \(Tell w) -> do
            lift $ tell w
            CPS.tell w

listenT' ::
    forall w es a fr u c.
    ( Monoid w
    , Freer c fr
    , Union u
    , Member u (Tell w) es
    , Applicative (Eff u fr '[] es)
    , c (Strict.WriterT w (Eff u fr '[] es))
    ) =>
    Eff u fr '[] es a ->
    Eff u fr '[] es (w, a)
listenT' m =
    swap <$> Strict.runWriterT do
        m & interposeFin @(Tell w) (liftStrictWriterT . injectF) \(Tell w) -> do
            liftStrictWriterT (tell w) *> tellStrictWriterT w


interpretTell ::
    (Monoid w, Freer c fr, Union u, Monad (Eff u fr '[] r), c (CPS.WriterT w (Eff u fr '[] r))) =>
    Eff u fr '[] (LTell w ': r) a ->
    Eff u fr '[] r (w, a)
interpretTell = fmap swap . CPS.runWriterT . interpretTellT
{-# INLINE interpretTell #-}

interpretTellT ::
    (Monoid w, Freer c fr, Union u, Monad (Eff u fr '[] r), c (CPS.WriterT w (Eff u fr '[] r))) =>
    Eff u fr '[] (LTell w ': r) ~> CPS.WriterT w (Eff u fr '[] r)
interpretTellT = interpretT \(Tell w) -> CPS.tell w
{-# INLINE interpretTellT #-}

interpretTell' ::
    (Monoid w, Freer c fr, Union u, Applicative (Eff u fr '[] r), c (Strict.WriterT w (Eff u fr '[] r))) =>
    Eff u fr '[] (LTell w ': r) a ->
    Eff u fr '[] r (w, a)
interpretTell' = fmap swap . Strict.runWriterT . interpretTellT'
{-# INLINE interpretTell' #-}

interpretTellT' ::
    (Monoid w, Freer c fr, Union u, Applicative (Eff u fr '[] r), c (Strict.WriterT w (Eff u fr '[] r))) =>
    Eff u fr '[] (LTell w ': r) ~> Strict.WriterT w (Eff u fr '[] r)
interpretTellT' = interpretFin (liftStrictWriterT . injectF) \(Tell w) -> tellStrictWriterT w
{-# INLINE interpretTellT' #-}

interpretTellK ::
    (Monoid w, MonadFreer c fr, Union u, c (Eff u fr '[] r)) =>
    Eff u fr '[] (LTell w ': r) a ->
    Eff u fr '[] r (w, a)
interpretTellK =
    interpretK (pure . (mempty,)) \k (Tell w) -> do
        (w', r) <- k ()
        pure (w <> w', r)


liftStrictWriterT :: forall w f. (Monoid w, Functor f) => f ~> Strict.WriterT w f
liftStrictWriterT = Strict.WriterT . ((,mempty) <$>)
{-# INLINE liftStrictWriterT #-}

tellStrictWriterT :: forall w f. Applicative f => w -> Strict.WriterT w f ()
tellStrictWriterT = Strict.WriterT . pure . ((),)
{-# INLINE tellStrictWriterT #-}


transactWriter ::
    forall w es a fr u c.
    ( Monoid w
    , Freer c fr
    , Union u
    , Member u (Tell w) es
    , Monad (Eff u fr '[] es)
    , c (CPS.WriterT w (Eff u fr '[] es))
    ) =>
    Eff u fr '[] es a ->
    Eff u fr '[] es a
transactWriter m = do
    (a, w) <- CPS.runWriterT $ confiscateT m
    tell @w w
    pure a

confiscateT ::
    forall w es a fr u c.
    ( Monoid w
    , Freer c fr
    , Union u
    , Member u (Tell w) es
    , Monad (Eff u fr '[] es)
    , c (CPS.WriterT w (Eff u fr '[] es))
    ) =>
    Eff u fr '[] es a ->
    CPS.WriterT w (Eff u fr '[] es) a
confiscateT m =
    m & interposeT @(Tell w) \(Tell w) -> CPS.tell w
