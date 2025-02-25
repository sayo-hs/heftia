-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2023 Sayo contributors
License     :  MPL-2.0 (see the LICENSE file)
Maintainer  :  ymdfield@outlook.jp

Interpreters for the [Except]("Data.Effect.Except") effects.
-}
module Control.Monad.Hefty.Except (
    module Control.Monad.Hefty.Except,
    module Data.Effect.Except,
)
where

import Control.Monad.Hefty (
    Eff,
    FOEs,
    Handler,
    interposeWith,
    interpret,
    interpretBy,
    (&),
    type (:>),
    type (~~>),
 )
import Data.Effect.Except

-- | Interpret the t'Throw'/t'Catch' effects.
runExcept :: forall e es a. (FOEs es) => Eff (Catch e ': Throw e ': es) a -> Eff es (Either e a)
runExcept = runThrow . runCatch
{-# INLINE runExcept #-}

-- | Interpret the t'Throw' effect.
runThrow :: forall e es a. (FOEs es) => Eff (Throw e ': es) a -> Eff es (Either e a)
runThrow = interpretBy (pure . Right) handleThrow
{-# INLINE runThrow #-}

-- | Interpret the t'Catch' effect.
runCatch :: forall e es a. (Throw e :> es, FOEs es) => Eff (Catch e ': es) a -> Eff es a
runCatch = interpret elabCatch
{-# INLINE runCatch #-}

-- | A handler for the t'Throw' effect.
handleThrow :: forall e f g a. (Applicative g) => Handler (Throw e) f g (Either e a)
handleThrow (Throw e) _ = pure $ Left e
{-# INLINE handleThrow #-}

-- | A handler for the t'Catch' effect.
elabCatch :: forall e es. (Throw e :> es, FOEs es) => Catch e ~~> Eff es
elabCatch (Catch action hdl) = action & interposeWith \(Throw e) _ -> hdl e
{-# INLINE elabCatch #-}
