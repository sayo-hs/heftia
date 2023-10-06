-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2023 Yamada Ryo
               (c) 2017 FP Complete
               (c) 2022 Fumiaki Kinoshita
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable

An elaborator for the t'Control.Effect.Class.Resource.Resource' effect class.
-}
module Control.Effect.Handler.Heftia.Resource where

import Control.Effect.Class.Resource (ResourceS (Bracket, BracketOnExcept))
import Control.Effect.Heftia (Elaborator)
import UnliftIO (MonadUnliftIO, bracket, bracketOnError)

-- | Elaborates the `Resource` effect under the `MonadUnliftIO` context.
resourceToIO :: MonadUnliftIO m => Elaborator ResourceS m
resourceToIO = \case
    Bracket acquire release thing -> bracket acquire release thing
    BracketOnExcept acquire onError thing -> bracketOnError acquire onError thing
