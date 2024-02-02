-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2023 Yamada Ryo
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable

Interpreter and elaborator for the t'Control.Effect.Class.Except.Except' effect class.
-}
module Control.Effect.Handler.Heftia.Except where

import Control.Effect (type (~>))
import Control.Effect.Class.Except (CatchS (Catch), ThrowI (Throw))
import Control.Effect.Freer (Fre, interposeK, interposeT, interpretK, interpretT, type (<|))
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT, throwE)
import Data.Function ((&))

-- | Elaborate the 'Catch' effect using the 'ExceptT' monad transformer.
elaborateExceptT ::
    (ThrowI e <| es, Monad m) =>
    CatchS e (Fre es m) ~> Fre es m
elaborateExceptT (Catch action (hdl :: e -> Fre es m a)) = do
    r <- runExceptT $ action & interposeT \(Throw (e :: e)) -> throwE e
    case r of
        Left e -> hdl e
        Right a -> pure a

{- |
Elaborate the 'Catch' effect using the t'Control.Monad.Trans.Cont.ContT' continuation monad
transformer.
-}
elaborateExceptK ::
    (ThrowI e <| es, Monad m) =>
    CatchS e (Fre es m) ~> Fre es m
elaborateExceptK (Catch action (hdl :: e -> Fre es m a)) =
    action & interposeK pure \_ (Throw (e :: e)) -> hdl e

-- | Interpret the 'Throw' effect using the 'ExceptT' monad transformer.
interpretThrowT :: Monad m => Fre (ThrowI e ': es) m ~> ExceptT e (Fre es m)
interpretThrowT = interpretT \(Throw e) -> throwE e
{-# INLINE interpretThrowT #-}

{- |
Interpret the 'Throw' effect using the t'Control.Monad.Trans.Cont.ContT' continuation monad
transformer.
-}
interpretThrowK :: Monad m => Fre (ThrowI e ': es) m a -> Fre es m (Either e a)
interpretThrowK = interpretK (pure . Right) \_ (Throw e) -> pure $ Left e
