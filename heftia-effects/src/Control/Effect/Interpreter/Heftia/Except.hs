-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2023 Sayo Koyoneda
License     :  MPL-2.0 (see the LICENSE file)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable

Interpreters for the t'Data.Effect.Except.Throw' / t'Data.Effect.Except.Catch' effects.
-}
module Control.Effect.Interpreter.Heftia.Except where

import Control.Effect (type (~>))
import Control.Exception (Exception)
import Control.Monad.Hefty.Interpret (interposeBy, interpretBy, interpretRec, interpretRecH)
import Control.Monad.Hefty.Types (Eff, Elab, Interpreter)
import Data.Effect.Except (Catch (Catch), Throw (Throw))
import Data.Effect.OpenUnion.Internal.FO (type (<|))
import Data.Effect.OpenUnion.Internal.HO (HFunctors, type (<<|))
import Data.Effect.Unlift (UnliftIO)
import Data.Function ((&))
import UnliftIO (throwIO)
import UnliftIO qualified as IO

runExcept :: Eff '[Catch e] (Throw e ': r) a -> Eff '[] r (Either e a)
runExcept = runThrow . runCatch

runThrow :: Eff '[] (Throw e ': r) a -> Eff '[] r (Either e a)
runThrow = interpretBy (pure . Right) handleThrow

runCatch :: (Throw e <| ef) => Eff '[Catch e] ef ~> Eff '[] ef
runCatch = interpretRecH elabCatch

handleThrow :: Interpreter (Throw e) (Eff '[] r) (Either e a)
handleThrow (Throw e) _ = pure $ Left e
{-# INLINE handleThrow #-}

elabCatch :: (Throw e <| ef) => Elab (Catch e) (Eff '[] ef)
elabCatch (Catch action hdl) = action & interposeBy pure \(Throw e) _ -> hdl e
{-# INLINE elabCatch #-}

runThrowIO
    :: forall e eh ef
     . (IO <| ef, Exception e, HFunctors eh)
    => Eff eh (Throw e ': ef) ~> Eff eh ef
runThrowIO = interpretRec \(Throw e) -> throwIO e

runCatchIO
    :: forall e eh ef
     . (UnliftIO <<| eh, IO <| ef, Exception e, HFunctors eh)
    => Eff (Catch e ': eh) ef ~> Eff eh ef
runCatchIO = interpretRecH \(Catch action hdl) -> IO.catch action hdl
