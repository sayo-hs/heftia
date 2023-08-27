module Control.Free.Trans.Freer where

import Control.Free (Instruction)
import Data.Kind (Type)

newtype TransFreer (f :: (Type -> Type) -> Instruction -> Type -> Type) ins m a = TransFreer
    {getTransFreer :: f m ins a}
    deriving newtype (Functor, Applicative, Monad)
    deriving stock (Foldable, Traversable)
