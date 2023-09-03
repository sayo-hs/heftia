-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Effect.Handler.Heftia.State where

import Control.Effect.Class.Machinery.HFunctor (HFunctor)
import Control.Effect.Class.State (StateS, pattern GetS, pattern PutS)
import Control.Heftia.Final (Hef)
import Control.Heftia.Trans.Final (HeftiaFinalT, liftLowerFinal)
import Control.Monad.State (get, put, runStateT)
import Control.Monad.Trans.Heftia (interpretTT)
import Data.Hefty.Sum (Sum)
import Data.Tuple (swap)

interpretState :: forall s es a. HFunctor (Sum es) => s -> Hef (StateS s ': es) a -> Hef es (s, a)
interpretState s a =
    fmap swap $
        (`runStateT` s) $
            interpretTT @(HeftiaFinalT Monad) @_ @_ @(StateS s)
                ( \case
                    GetS -> get
                    PutS s' -> put s'
                )
                (liftLowerFinal undefined)
