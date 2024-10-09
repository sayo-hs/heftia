-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2024 Sayo Koyoneda
License     :  MPL-2.0 (see the LICENSE file)
Maintainer  :  ymdfield@outlook.jp
Portability :  portable
-}
module Control.Effect.Interpreter.Heftia.Input where

import Control.Arrow ((>>>))
import Control.Effect.Interpreter.Heftia.State (evalState)
import Control.Monad.Hefty (Eff, interpret, raiseUnder, type (~>))
import Data.Effect.Input (Input (Input))
import Data.Effect.State (gets, put)
import Data.List (uncons)

runInputEff
    :: forall i ef eh
     . Eff eh ef i
    -> Eff eh (Input i ': ef) ~> Eff eh ef
runInputEff a = interpret \Input -> a

runInputConst
    :: forall i ef eh
     . i
    -> Eff eh (Input i ': ef) ~> Eff eh ef
runInputConst i = interpret \Input -> pure i

runInputList :: forall i r. [i] -> Eff '[] (Input (Maybe i) ': r) ~> Eff '[] r
runInputList is =
    raiseUnder
        >>> int
        >>> evalState is
  where
    int = interpret \Input -> do
        is' <- gets @[i] uncons
        mapM_ (put . snd) is'
        pure $ fst <$> is'
