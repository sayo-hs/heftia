-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2023 Sayo contributors
               (c) 2017 FP Complete
               (c) 2022 Fumiaki Kinoshita
License     :  MPL-2.0 (see the LICENSE file)
Maintainer  :  ymdfield@outlook.jp
Portability :  portable

An elaborator for the t'Control.Effect.Class.Resource.Resource' effect class.
-}
module Control.Monad.Hefty.Resource (
    module Control.Monad.Hefty.Resource,
    module Data.Effect.Resource,
)
where

import Control.Effect (type (~>))
import Control.Monad.Hefty.Interpret (interpretH)
import Control.Monad.Hefty.Types (Eff, type (~~>))
import Data.Effect.OpenUnion.Internal.FO (type (<|))
import Data.Effect.OpenUnion.Internal.HO (type (<<|))
import Data.Effect.Resource
import Data.Effect.Unlift (UnliftIO)
import UnliftIO (MonadUnliftIO)
import UnliftIO qualified as IO

-- | Elaborates the `Resource` effect under the `UnliftIO` context.
runResourceIO
    :: (UnliftIO <<| eh, IO <| ef)
    => Eff (Resource ': eh) ef ~> Eff eh ef
runResourceIO = interpretH elabResourceIO

elabResourceIO :: (MonadUnliftIO m) => Resource ~~> m
elabResourceIO = \case
    Bracket acquire release thing -> IO.bracket acquire release thing
    BracketOnExcept acquire onError thing -> IO.bracketOnError acquire onError thing
{-# INLINE elabResourceIO #-}
