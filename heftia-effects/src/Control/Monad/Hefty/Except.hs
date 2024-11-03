-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2023 Sayo Koyoneda
License     :  MPL-2.0 (see the LICENSE file)
Maintainer  :  ymdfield@outlook.jp

Interpreters for the [Except]("Data.Effect.Except") effects.
-}
module Control.Monad.Hefty.Except (
    module Control.Monad.Hefty.Except,
    module Data.Effect.Except,
)
where

import Control.Exception (Exception)
import Control.Monad.Hefty (
    Eff,
    Interpreter,
    interposeWith,
    interpret,
    interpretBy,
    interpretH,
    (&),
    type (<<|),
    type (<|),
    type (~>),
    type (~~>),
 )
import Data.Effect.Except
import Data.Effect.Unlift (UnliftIO)
import UnliftIO (throwIO)
import UnliftIO qualified as IO

-- | Interpret the t'Throw'/t'Catch' effects.
runExcept :: Eff '[Catch e] (Throw e ': r) a -> Eff '[] r (Either e a)
runExcept = runThrow . runCatch

-- | Interpret the t'Throw' effect.
runThrow :: Eff '[] (Throw e ': r) a -> Eff '[] r (Either e a)
runThrow = interpretBy (pure . Right) handleThrow

-- | Interpret the t'Catch' effect.
runCatch :: (Throw e <| ef) => Eff '[Catch e] ef ~> Eff '[] ef
runCatch = interpretH elabCatch

-- | A handler function for the t'Throw' effect.
handleThrow :: Interpreter (Throw e) (Eff '[] r) (Either e a)
handleThrow (Throw e) _ = pure $ Left e
{-# INLINE handleThrow #-}

-- | A elaborator function for the t'Catch' effect.
elabCatch :: (Throw e <| ef) => Catch e ~~> Eff '[] ef
elabCatch (Catch action hdl) = action & interposeWith \(Throw e) _ -> hdl e
{-# INLINE elabCatch #-}

-- | Interpret the t'Throw' effect based on an IO-fused semantics using IO-level exceptions.
runThrowIO
    :: forall e eh ef
     . (IO <| ef, Exception e)
    => Eff eh (Throw e ': ef) ~> Eff eh ef
runThrowIO = interpret \(Throw e) -> throwIO e

-- | Interpret the t'Catch' effect based on an IO-fused semantics using IO-level exceptions.
runCatchIO
    :: forall e eh ef
     . (UnliftIO <<| eh, IO <| ef, Exception e)
    => Eff (Catch e ': eh) ef ~> Eff eh ef
runCatchIO = interpretH \(Catch action hdl) -> IO.catch action hdl
