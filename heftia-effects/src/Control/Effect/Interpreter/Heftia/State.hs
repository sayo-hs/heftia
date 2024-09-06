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

Interpreter for the t'Control.Effect.Class.State.State' effect class.
-}
module Control.Effect.Interpreter.Heftia.State where

import Control.Arrow ((>>>))
import Control.Effect (type (~>))
import Control.Effect.Hefty (Eff, injectF, interpose, interposeT, interpret, interpretFin, interpretK, raiseUnder)
import Control.Effect.Interpreter.Heftia.Reader (runAsk)
import Control.Freer (Freer)
import Control.Monad.Freer (MonadFreer)
import Control.Monad.State (StateT)
import Control.Monad.Trans.State qualified as T
import Data.Effect.HFunctor (HFunctor)
import Data.Effect.Reader (Ask (Ask), LAsk, ask)
import Data.Effect.State (LState, State (Get, Put), get, put)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Hefty.Union (Member, Union)
import Data.Tuple (swap)
import UnliftIO (MonadIO, newIORef, readIORef, writeIORef)

-- | Interpret the 'Get'/'Put' effects using the 'StateT' monad transformer.
runState ::
    forall s r a fr u c.
    (Freer c fr, Union u, c (Eff u fr '[] r), c (StateT s (Eff u fr '[] r)), Applicative (Eff u fr '[] r)) =>
    s ->
    Eff u fr '[] (LState s ': r) a ->
    Eff u fr '[] r (s, a)
runState s a = swap <$> T.runStateT (runStateT a) s
{-# INLINE runState #-}

evalState ::
    forall s r fr u c.
    (Freer c fr, Union u, c (Eff u fr '[] r), c (StateT s (Eff u fr '[] r)), Applicative (Eff u fr '[] r)) =>
    s ->
    Eff u fr '[] (LState s ': r) ~> Eff u fr '[] r
evalState s a = snd <$> runState s a
{-# INLINE evalState #-}

execState ::
    forall s r a fr u c.
    (Freer c fr, Union u, c (Eff u fr '[] r), c (StateT s (Eff u fr '[] r)), Applicative (Eff u fr '[] r)) =>
    s ->
    Eff u fr '[] (LState s ': r) a ->
    Eff u fr '[] r s
execState s a = fst <$> runState s a
{-# INLINE execState #-}

-- | Interpret the 'Get'/'Put' effects using the 'StateT' monad transformer.
runStateT ::
    forall s r fr u c.
    (Freer c fr, Union u, c (StateT s (Eff u fr '[] r)), c (Eff u fr '[] r), Applicative (Eff u fr '[] r)) =>
    Eff u fr '[] (LState s ': r) ~> StateT s (Eff u fr '[] r)
runStateT =
    interpretFin (\u -> T.StateT \s -> (,s) <$> injectF u) fuseStateEffect

-- | Interpret the 'Get'/'Put' effects using delimited continuations.
runStateK ::
    forall s r a fr u c.
    ( MonadFreer c fr
    , Union u
    , HFunctor (u '[])
    , Member u (Ask s) (LAsk s ': r)
    , c (Eff u fr '[] (LAsk s ': r))
    , Applicative (Eff u fr '[] r)
    ) =>
    s ->
    Eff u fr '[] (LState s ': r) a ->
    Eff u fr '[] r (s, a)
runStateK initialState =
    raiseUnder
        >>> interpretK
            (\a -> ask <&> (,a))
            ( \k -> \case
                Get -> k =<< ask
                Put s -> k () & interpose @(Ask s) \Ask -> pure s
            )
        >>> runAsk initialState

runStateIORef ::
    forall s r a fr u c.
    (Freer c fr, Union u, MonadIO (Eff u fr '[] r)) =>
    s ->
    Eff u fr '[] (LState s ': r) a ->
    Eff u fr '[] r (s, a)
runStateIORef s m = do
    ref <- newIORef s
    a <-
        m & interpret \case
            Get -> readIORef ref
            Put s' -> writeIORef ref s'
    readIORef ref <&> (,a)

transactState ::
    forall s r fr u c.
    (Freer c fr, Union u, Member u (State s) r, Monad (Eff u fr '[] r), c (StateT s (Eff u fr '[] r))) =>
    Eff u fr '[] r ~> Eff u fr '[] r
transactState m = do
    pre <- get @s
    (a, post) <- T.runStateT (interposeT fuseStateEffect m) pre
    put post
    pure a

fuseStateEffect :: Applicative f => State s ~> StateT s f
fuseStateEffect = \case
    Get -> T.StateT \s -> pure (s, s)
    Put s -> T.StateT \_ -> pure ((), s)
