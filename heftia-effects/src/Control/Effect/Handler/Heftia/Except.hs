-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2023 Yamada Ryo
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable

Interpreter and elaborator for the t'Data.Effect.Except.Throw' / t'Data.Effect.Except.Catch' effect
classes.
-}
module Control.Effect.Handler.Heftia.Except where

import Control.Effect (type (~>))
import Control.Effect.Hefty (
    Eff,
    Elab,
    interposeK,
    interposeT,
    interpretK,
    interpretT,
 )
import Control.Monad.Freer (MonadFreer)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.Effect.Except (Catch (Catch), LThrow, Throw (Throw))
import Data.Function ((&))
import Data.Hefty.Union (Union, Member)

-- | Elaborate the t'Catch' effect using the 'ExceptT' monad transformer.
elaborateCatch ::
    forall e ef fr u c.
    ( Member u (Throw e) ef
    , MonadFreer c fr
    , Union u
    , c (Eff u fr '[] ef)
    , c (ExceptT e (Eff u fr '[] ef))
    ) =>
    Elab (Catch e) (Eff u fr '[] ef)
elaborateCatch (Catch action hdl) = do
    r <- runExceptT $ action & interposeT \(Throw e) -> throwE e
    case r of
        Left e -> hdl e
        Right a -> pure a

-- | Elaborate the 'Catch' effect using a delimited continuation.
elaborateCatchK ::
    forall e ef fr u c.
    (Member u (Throw e) ef, MonadFreer c fr, Union u, c (Eff u fr '[] ef)) =>
    Elab (Catch e) (Eff u fr '[] ef)
elaborateCatchK (Catch action hdl) =
    action & interposeK pure \_ (Throw e) -> hdl e

-- | Interpret the 'Throw' effect using the 'ExceptT' monad transformer.
interpretThrow ::
    forall e r a fr u c.
    (MonadFreer c fr, Union u, c (Eff u fr '[] r), c (ExceptT e (Eff u fr '[] r))) =>
    Eff u fr '[] (LThrow e ': r) a ->
    Eff u fr '[] r (Either e a)
interpretThrow = runExceptT . interpretThrowT
{-# INLINE interpretThrow #-}

-- | Interpret the 'Throw' effect using the 'ExceptT' monad transformer.
interpretThrowT ::
    forall e r fr u c.
    (MonadFreer c fr, Union u, c (Eff u fr '[] r), c (ExceptT e (Eff u fr '[] r))) =>
    Eff u fr '[] (LThrow e ': r) ~> ExceptT e (Eff u fr '[] r)
interpretThrowT = interpretT \(Throw e) -> throwE e
{-# INLINE interpretThrowT #-}

-- | Interpret the 'Throw' effect using a delimited continuation.
interpretThrowK ::
    forall e r a fr u c.
    (MonadFreer c fr, Union u, c (Eff u fr '[] r)) =>
    Eff u fr '[] (LThrow e ': r) a ->
    Eff u fr '[] r (Either e a)
interpretThrowK = interpretK (pure . Right) \_ (Throw e) -> pure $ Left e
