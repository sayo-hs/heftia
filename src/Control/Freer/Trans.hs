module Control.Freer.Trans where

import Control.Free (Instruction)
import Data.Kind (Type)

newtype FreerT (f :: (Type -> Type) -> Instruction -> Type -> Type) ins m a = FreerT
    {runFreerT :: f m ins a}
    deriving newtype (Functor, Applicative, Monad)
    deriving stock (Foldable, Traversable)
