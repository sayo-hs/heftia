module Data.Free.Sum where

import Data.Kind (Type)

data NopF (a :: Type)
    deriving (Functor, Foldable, Traversable)
