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

import Control.Arrow ((>>>))
import Control.Effect (type (~>))
import Control.Effect.Hefty (
    Eff,
    Elab,
    interposeK,
    interposeT,
    interpretK,
    interpretRec,
    interpretRecH,
    interpretT,
 )
import Control.Exception (Exception)
import Control.Monad.Freer (MonadFreer)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.Effect.Except (Catch (Catch), LThrow, Throw (Throw))
import Data.Effect.HFunctor (HFunctor)
import Data.Effect.Unlift (UnliftIO)
import Data.Function ((&))
import Data.Hefty.Extensible (ForallHFunctor, type (<<|), type (<|))
import Data.Hefty.Extensible qualified as Ex
import Data.Hefty.Union (Member, Union)
import UnliftIO (throwIO)
import UnliftIO qualified as IO

-- | Interpret the "Data.Effect.Except" effects using the 'ExceptT' monad transformer internally.
runExcept ::
    forall e a ef fr u c.
    ( Member u (Throw e) (LThrow e ': ef)
    , MonadFreer c fr
    , Union u
    , c (Eff u fr '[] (LThrow e ': ef))
    , c (ExceptT e (Eff u fr '[] (LThrow e ': ef)))
    , HFunctor (u '[Catch e])
    , c (Eff u fr '[] ef)
    , c (ExceptT e (Eff u fr '[] ef))
    , HFunctor (u '[])
    ) =>
    Eff u fr '[Catch e] (LThrow e ': ef) a ->
    Eff u fr '[] ef (Either e a)
runExcept = runCatch >>> runThrow
{-# INLINE runExcept #-}

-- | Elaborate the t'Catch' effect using the 'ExceptT' monad transformer internally.
runCatch ::
    forall e ef fr u c.
    ( Member u (Throw e) ef
    , MonadFreer c fr
    , Union u
    , c (Eff u fr '[] ef)
    , c (ExceptT e (Eff u fr '[] ef))
    , HFunctor (u '[Catch e])
    , HFunctor (u '[])
    ) =>
    Eff u fr '[Catch e] ef ~> Eff u fr '[] ef
runCatch = interpretRecH elabCatch
{-# INLINE runCatch #-}

elabCatch ::
    forall e ef fr u c.
    ( Member u (Throw e) ef
    , MonadFreer c fr
    , Union u
    , c (Eff u fr '[] ef)
    , c (ExceptT e (Eff u fr '[] ef))
    ) =>
    Elab (Catch e) (Eff u fr '[] ef)
elabCatch (Catch action hdl) = do
    r <- runExceptT $ action & interposeT \(Throw e) -> throwE e
    case r of
        Left e -> hdl e
        Right a -> pure a

-- | Elaborate the 'Catch' effect using a delimited continuation.
elabCatchK ::
    forall e ef fr u c.
    (Member u (Throw e) ef, MonadFreer c fr, Union u, c (Eff u fr '[] ef)) =>
    Elab (Catch e) (Eff u fr '[] ef)
elabCatchK (Catch action hdl) =
    action & interposeK pure \_ (Throw e) -> hdl e

-- | Interpret the 'Throw' effect using the 'ExceptT' monad transformer.
runThrow ::
    forall e r a fr u c.
    (MonadFreer c fr, Union u, c (Eff u fr '[] r), c (ExceptT e (Eff u fr '[] r))) =>
    Eff u fr '[] (LThrow e ': r) a ->
    Eff u fr '[] r (Either e a)
runThrow = runExceptT . runThrowT
{-# INLINE runThrow #-}

-- | Interpret the 'Throw' effect using the 'ExceptT' monad transformer.
runThrowT ::
    forall e r fr u c.
    (MonadFreer c fr, Union u, c (Eff u fr '[] r), c (ExceptT e (Eff u fr '[] r))) =>
    Eff u fr '[] (LThrow e ': r) ~> ExceptT e (Eff u fr '[] r)
runThrowT = interpretT \(Throw e) -> throwE e
{-# INLINE runThrowT #-}

-- | Interpret the 'Throw' effect using a delimited continuation.
runThrowK ::
    forall e r a fr u c.
    (MonadFreer c fr, Union u, c (Eff u fr '[] r)) =>
    Eff u fr '[] (LThrow e ': r) a ->
    Eff u fr '[] r (Either e a)
runThrowK = interpretK (pure . Right) \_ (Throw e) -> pure $ Left e

runThrowIO ::
    forall e eh ef fr c.
    (MonadFreer c fr, IO <| ef, ForallHFunctor eh, Exception e) =>
    Ex.Eff fr eh (LThrow e ': ef) ~> Ex.Eff fr eh ef
runThrowIO = interpretRec \(Throw e) -> throwIO e
{-# INLINE runThrowIO #-}

runCatchIO ::
    forall e eh ef fr c.
    (MonadFreer c fr, UnliftIO <<| eh, IO <| ef, ForallHFunctor eh, Exception e) =>
    Ex.Eff fr (Catch e ': eh) ef ~> Ex.Eff fr eh ef
runCatchIO = interpretRecH \(Catch action hdl) -> IO.catch action hdl
{-# INLINE runCatchIO #-}
