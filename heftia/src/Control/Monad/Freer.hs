{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Monad.Freer where

import Control.Effect (type (~>))
import Control.Freer (Freer, interpretFreer)
import Control.Monad.Cont (Cont)

class Freer Monad fr => MonadFreer fr where
    interpretFreerK :: (e ~> Cont r) -> fr e ~> Cont r
    interpretFreerK i = interpretFreer i
    {-# INLINE interpretFreerK #-}

{-
class (Freer c fr, forall f. c f => Monad f) => MonadFreer c fr where
    interpretFreerK :: (e ~> Cont r) -> fr e ~> Cont r
    default interpretFreerK :: c (Cont r) => (e ~> Cont r) -> fr e ~> Cont r
    interpretFreerK i = interpretFreer i
    {-# INLINE interpretFreerK #-}
-}
