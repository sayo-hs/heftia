{-# LANGUAGE AllowAmbiguousTypes #-}

-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2023 Sayo Koyoneda
License     :  MPL-2.0 (see the LICENSE file)
Maintainer  :  ymdfield@outlook.jp
Portability :  portable

Interpreter for the t'Data.Effect.State.State' effect.
-}
module Control.Effect.Interpreter.Heftia.State where

import Control.Arrow ((>>>))
import Control.Effect (type (~>))
import Control.Effect.Interpreter.Heftia.Reader (runAsk)
import Control.Monad.Hefty (interpose, interpretBy, interpretRecWith, raiseUnder)
import Control.Monad.Hefty.Interpret (interpret)
import Control.Monad.Hefty.Interpret.State (
    StateInterpreter,
    interposeStateBy,
    interpretStateBy,
    interpretStateRecWith,
 )
import Control.Monad.Hefty.Types (Eff)
import Data.Effect.OpenUnion.Internal.FO (type (<|))
import Data.Effect.OpenUnion.Internal.HO (HFunctors)
import Data.Effect.Reader (Ask (Ask), ask)
import Data.Effect.State (State (Get, Put), get, put)
import Data.Function ((&))
import Data.Functor ((<&>))
import UnliftIO (newIORef, readIORef, writeIORef)

-- | Interpret the 'Get'/'Put' effects.
runState :: forall s ef a. s -> Eff '[] (State s ': ef) a -> Eff '[] ef (s, a)
runState s0 = interpretStateBy s0 (curry pure) handleState

evalState :: forall s ef a. s -> Eff '[] (State s ': ef) a -> Eff '[] ef a
evalState s0 = interpretStateBy s0 (const pure) handleState

execState :: forall s ef a. s -> Eff '[] (State s ': ef) a -> Eff '[] ef s
execState s0 = interpretStateBy s0 (\s _ -> pure s) handleState

runStateRec :: forall s ef eh. (HFunctors eh) => s -> Eff eh (State s ': ef) ~> Eff eh ef
runStateRec s0 = interpretStateRecWith s0 handleState

handleState :: StateInterpreter s (State s) (Eff eh r) ans
handleState = \case
    Put s -> \_ k -> k s ()
    Get -> \s k -> k s s
{-# INLINE handleState #-}

runStateIORef
    :: forall s ef eh a
     . (IO <| ef, HFunctors eh)
    => s
    -> Eff eh (State s ': ef) a
    -> Eff eh ef (s, a)
runStateIORef s0 m = do
    ref <- newIORef s0
    a <-
        m & interpret \case
            Get -> readIORef ref
            Put s -> writeIORef ref s
    readIORef ref <&> (,a)

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

-- | A naive but somewhat slower version of 'runStateRec' that does not use ad-hoc optimizations.
runStateNaiveRec :: forall s ef eh. (HFunctors eh) => s -> Eff eh (State s ': ef) ~> Eff eh ef
runStateNaiveRec s0 =
    raiseUnder
        >>> interpretRecWith \case
            Get -> (ask @s >>=)
            Put s -> \k -> k () & interpose @(Ask s) \Ask -> pure s
        >>> runAsk @s s0
