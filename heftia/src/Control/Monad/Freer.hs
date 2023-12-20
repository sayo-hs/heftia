{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Monad.Freer where

import Control.Effect.Class (type (~>))
import Control.Freer (Freer, interpretFreer)
import Control.Monad.Cont (Cont)

class Freer Monad f => MonadFreer f where
    interpretFreerK :: (e ~> Cont r) -> f e ~> Cont r
    interpretFreerK i = interpretFreer i
    {-# INLINE interpretFreerK #-}
