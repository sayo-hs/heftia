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
    Effectful,
    MemberF,
    interposeK,
    interposeT,
    interpretK,
    interpretT,
 )
import Control.Monad.Freer (MonadFreer)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.Effect (LNop)
import Data.Effect.Except (Catch (Catch), Throw (Throw))
import Data.Free.Sum (type (+))
import Data.Function ((&))
import Data.Hefty.Union (Union)

-- | Elaborate the 'Catch' effect using a delimited continuation.
elaborateCatch ::
    (MemberF u (Throw e) ef, MonadFreer f, Union u) =>
    Catch e (Effectful u f LNop ef) ~> Effectful u f LNop ef
elaborateCatch (Catch action hdl) =
    action & interposeK pure \_ (Throw e) -> hdl e

-- | Elaborate the 'Catch' effect using the 'ExceptT' monad transformer.
elaborateCatchT ::
    (MemberF u (Throw e) ef, MonadFreer f, Union u) =>
    Catch e (Effectful u f LNop ef) ~> Effectful u f LNop ef
elaborateCatchT (Catch action hdl) = do
    r <- runExceptT $ action & interposeT \(Throw e) -> throwE e
    case r of
        Left e -> hdl e
        Right a -> pure a

-- | Interpret the 'Throw' effect using a delimited continuation.
interpretThrow ::
    (MonadFreer f, Union u) =>
    Effectful u f LNop (Throw e + es) a ->
    Effectful u f LNop es (Either e a)
interpretThrow = interpretK (pure . Right) \_ (Throw e) -> pure $ Left e

-- | Interpret the 'Throw' effect using the 'ExceptT' monad transformer.
interpretThrowT ::
    (MonadFreer f, Union u) =>
    Effectful u f LNop (Throw e + es) ~> ExceptT e (Effectful u f LNop es)
interpretThrowT = interpretT \(Throw e) -> throwE e
{-# INLINE interpretThrowT #-}
