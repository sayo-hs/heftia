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
import Control.Effect.Hefty (Eff, Elab, injectF, interposeFin, interposeT, interpretFin, interpretK, interpretT, rewrite)
import Control.Freer (Freer)
import Control.Monad.Freer (MonadFreer)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Writer.CPS qualified as CPS
import Control.Monad.Trans.Writer.Strict qualified as Strict
import Data.Effect.HFunctor (HFunctor)
import Data.Effect.Writer (LTell, Tell (Tell), WriterH (Censor, Listen), tell)
import Data.Function ((&))
import Data.Hefty.Union (Member, Union)
import Data.Tuple (swap)

elabWriterPost ::
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
elabWriterPost = \case
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
    (w -> w) ->
    Eff u fr '[] es ~> Eff u fr '[] es
postCensor f m = do
    (a, w) <- CPS.runWriterT $ confiscateT m
    tell $ f w
    pure a

elabWriterPre ::
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
elabWriterPre = \case
    Listen m -> listenT m
    Censor f m -> preCensor f m

elabWriterPre' ::
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
elabWriterPre' = \case
    Listen m -> listenT' m
    Censor f m -> preCensor f m

preCensor ::
    forall w es fr u c.
    (Freer c fr, Member u (Tell w) es, Union u, HFunctor (u '[])) =>
    (w -> w) ->
    Eff u fr '[] es ~> Eff u fr '[] es
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

runTell ::
    (Monoid w, Freer c fr, Union u, Monad (Eff u fr '[] r), c (CPS.WriterT w (Eff u fr '[] r))) =>
    Eff u fr '[] (LTell w ': r) a ->
    Eff u fr '[] r (w, a)
runTell = fmap swap . CPS.runWriterT . runTellT
{-# INLINE runTell #-}

runTellT ::
    (Monoid w, Freer c fr, Union u, Monad (Eff u fr '[] r), c (CPS.WriterT w (Eff u fr '[] r))) =>
    Eff u fr '[] (LTell w ': r) ~> CPS.WriterT w (Eff u fr '[] r)
runTellT = interpretT \(Tell w) -> CPS.tell w
{-# INLINE runTellT #-}

runTell' ::
    (Monoid w, Freer c fr, Union u, Applicative (Eff u fr '[] r), c (Strict.WriterT w (Eff u fr '[] r))) =>
    Eff u fr '[] (LTell w ': r) a ->
    Eff u fr '[] r (w, a)
runTell' = fmap swap . Strict.runWriterT . runTellT'
{-# INLINE runTell' #-}

runTellT' ::
    (Monoid w, Freer c fr, Union u, Applicative (Eff u fr '[] r), c (Strict.WriterT w (Eff u fr '[] r))) =>
    Eff u fr '[] (LTell w ': r) ~> Strict.WriterT w (Eff u fr '[] r)
runTellT' = interpretFin (liftStrictWriterT . injectF) \(Tell w) -> tellStrictWriterT w
{-# INLINE runTellT' #-}

runTellK ::
    (Monoid w, MonadFreer c fr, Union u, c (Eff u fr '[] r)) =>
    Eff u fr '[] (LTell w ': r) a ->
    Eff u fr '[] r (w, a)
runTellK =
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
