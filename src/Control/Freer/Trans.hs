-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Freer.Trans where

import Control.Effect.Class (Instruction)
import Data.Kind (Type)

newtype FreerT (f :: (Type -> Type) -> Instruction -> Type -> Type) ins m a = FreerT
    {runFreerT :: f m ins a}
    deriving newtype (Functor, Applicative, Monad)
    deriving stock (Foldable, Traversable)
