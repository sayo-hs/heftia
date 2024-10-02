{-# LANGUAGE AllowAmbiguousTypes #-}

-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2023 Sayo Koyoneda
License     :  MPL-2.0 (see the LICENSE file)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable

Interpreter for the t'Data.Effect.State.State' effect.
-}
module Control.Effect.Interpreter.Heftia.State where

import Control.Effect (type (~>))
import Control.Monad.Hefty.Interpret (interpretRec)
import Control.Monad.Hefty.Interpret.State (
    StateInterpreter,
    interposeStateBy,
    interpretStateBy,
    interpretStateRecWith,
 )
import Control.Monad.Hefty.Types (Eff)
import Data.Effect.OpenUnion.Internal.FO (type (<|))
import Data.Effect.State (State (Get, Put), get, put)
import Data.Function ((&))
import Data.Functor ((<&>))
import UnliftIO (newIORef, readIORef, writeIORef)

-- | Interpret the 'Get'/'Put' effects.
runState :: forall s r a. s -> Eff '[] (State s ': r) a -> Eff '[] r (s, a)
runState s0 = interpretStateBy s0 (curry pure) handleState

evalState :: forall s r a. s -> Eff '[] (State s ': r) a -> Eff '[] r a
evalState s0 = interpretStateBy s0 (const pure) handleState

execState :: forall s r a. s -> Eff '[] (State s ': r) a -> Eff '[] r s
execState s0 = interpretStateBy s0 (\s _ -> pure s) handleState

runStateRec :: forall s r. s -> Eff '[] (State s ': r) ~> Eff '[] r
runStateRec s0 = interpretStateRecWith s0 handleState

handleState :: StateInterpreter s (State s) (Eff '[] r) ans
handleState = \case
    Put s -> \_ k -> k s ()
    Get -> \s k -> k s s
{-# INLINE handleState #-}

runStateIORef
    :: forall s r eh a
     . (IO <| r)
    => s
    -> Eff eh (State s ': r) a
    -> Eff eh r (s, a)
runStateIORef s0 m = do
    ref <- newIORef s0
    a <-
        m & interpretRec \case
            Get -> readIORef ref
            Put s -> writeIORef ref s
    readIORef ref <&> (,a)

transactState :: forall s ef. (State s <| ef) => Eff '[] ef ~> Eff '[] ef
transactState m = do
    pre <- get @s
    (post, a) <- interposeStateBy pre (curry pure) handleState m
    put post
    pure a
