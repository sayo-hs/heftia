-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2023 Sayo Koyoneda
               (c) 2017 FP Complete
               (c) 2022 Fumiaki Kinoshita
License     :  MPL-2.0 (see the LICENSE file)
Maintainer  :  ymdfield@outlook.jp
Portability :  portable

An elaborator for the t'Control.Effect.Class.Resource.Resource' effect class.
-}
module Control.Effect.Interpreter.Heftia.Resource where

import Control.Effect (type (~>))
import Control.Monad.Hefty.Interpret (interpretH)
import Control.Monad.Hefty.Types (Eff, type (~~>))
import Data.Effect.OpenUnion.Internal.FO (type (<|))
import Data.Effect.OpenUnion.Internal.HO (type (<<|))
import Data.Effect.Resource (Resource (Bracket, BracketOnExcept))
import Data.Effect.Unlift (UnliftIO)
import UnliftIO (MonadUnliftIO, bracket, bracketOnError)

-- | Elaborates the `Resource` effect under the `MonadUnliftIO` context.
runResourceIO
    :: (UnliftIO <<| eh, IO <| ef)
    => Eff (Resource ': eh) ef ~> Eff eh ef
runResourceIO = interpretH elabResourceIO

elabResourceIO :: (MonadUnliftIO m) => Resource ~~> m
elabResourceIO = \case
    Bracket acquire release thing -> bracket acquire release thing
    BracketOnExcept acquire onError thing -> bracketOnError acquire onError thing
{-# INLINE elabResourceIO #-}
