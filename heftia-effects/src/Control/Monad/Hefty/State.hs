{-# LANGUAGE AllowAmbiguousTypes #-}

-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2023 Sayo contributors
License     :  MPL-2.0 (see the LICENSE file)
Maintainer  :  ymdfield@outlook.jp

Interpreter for the t'Data.Effect.State.State' effect.
-}
module Control.Monad.Hefty.State (
    module Control.Monad.Hefty.State,
    module Data.Effect.State,
)
where

import Control.Arrow ((>>>))
import Control.Monad.Hefty (
    Eff,
    StateInterpreter,
    interpose,
    interposeStateBy,
    interpret,
    interpretBy,
    interpretH,
    interpretRecWith,
    interpretStateBy,
    interpretStateRecWith,
    raiseUnder,
    (&),
    type (<|),
    type (~>),
 )
import Control.Monad.Hefty.Reader (
    Ask (..),
    Local (..),
    ask,
    runAsk,
 )
import Data.Effect.State
import Data.Functor ((<&>))
import UnliftIO (newIORef, readIORef, writeIORef)

-- | Interpret the 'State' effect.
runState :: forall s ef a. s -> Eff '[] (State s ': ef) a -> Eff '[] ef (s, a)
runState s0 = interpretStateBy s0 (curry pure) handleState

-- | Interpret the 'State' effect. Do not include the final state in the return value.
evalState :: forall s ef a. s -> Eff '[] (State s ': ef) a -> Eff '[] ef a
evalState s0 = interpretStateBy s0 (const pure) handleState

-- | Interpret the 'State' effect. Do not include the final result in the return value.
execState :: forall s ef a. s -> Eff '[] (State s ': ef) a -> Eff '[] ef s
execState s0 = interpretStateBy s0 (\s _ -> pure s) handleState

{- |
Interpret the 'State' effect.

Interpretation is performed recursively with respect to the scopes of unelaborated higher-order effects @eh@.
Note that the state is reset and does not persist beyond the scopes.
-}
evalStateRec :: forall s ef eh. s -> Eff eh (State s ': ef) ~> Eff eh ef
evalStateRec s0 = interpretStateRecWith s0 handleState

-- | A handler function for the 'State' effect.
handleState :: StateInterpreter s (State s) (Eff eh r) ans
handleState = \case
    Put s -> \_ k -> k s ()
    Get -> \s k -> k s s
{-# INLINE handleState #-}

-- | Within the given scope, make the state roll back to the beginning of the scope in case of exceptions, etc.
transactState :: forall s ef. (State s <| ef) => Eff '[] ef ~> Eff '[] ef
transactState m = do
    pre <- get @s
    (post, a) <- interposeStateBy pre (curry pure) handleState m
    put post
    pure a

-- | A naive but somewhat slower version of 'runState' that does not use ad-hoc optimizations.
runStateNaive :: forall s ef a. s -> Eff '[] (State s ': ef) a -> Eff '[] ef (s, a)
runStateNaive s0 m = do
    f <-
        m & interpretBy (\a -> pure \s -> pure (s, a)) \case
            Get -> \k -> pure \s -> k s >>= ($ s)
            Put s -> \k -> pure \_ -> k () >>= ($ s)
    f s0

-- | A naive but somewhat slower version of 'evalStateRec' that does not use ad-hoc optimizations.
evalStateNaiveRec :: forall s ef eh. s -> Eff eh (State s ': ef) ~> Eff eh ef
evalStateNaiveRec s0 =
    raiseUnder
        >>> interpretRecWith \case
            Get -> (ask @s >>=)
            Put s -> \k -> k () & interpose @(Ask s) \Ask -> pure s
        >>> runAsk @s s0
