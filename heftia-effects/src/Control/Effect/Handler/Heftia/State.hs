-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Effect.Handler.Heftia.State where

import Control.Effect.Class (type (~>))
import Control.Effect.Class.State (StateI (Get, Put))
import Control.Effect.Freer (Fre, interpretT)
import Control.Monad.State (StateT)
import Control.Monad.Trans.State (runStateT)
import Control.Monad.Trans.State qualified as T
import Data.Tuple (swap)

-- | Interpret the 'Get'/'Put' effects using the 'StateT' monad transformer.
interpretState :: Monad m => s -> Fre (StateI s ': es) m a -> Fre es m (s, a)
interpretState s a = swap <$> runStateT (interpretStateT a) s
{-# INLINE interpretState #-}

-- | Interpret the 'Get'/'Put' effects using the 'StateT' monad transformer.
interpretStateT :: Monad m => Fre (StateI s ': es) m ~> StateT s (Fre es m)
interpretStateT = interpretT \case
    Get -> T.get
    Put s -> T.put s
{-# INLINE interpretStateT #-}
