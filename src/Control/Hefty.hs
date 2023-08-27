module Control.Hefty where

import Data.Kind (Type)

{- | The class for /signature/s (datatypes of higher-order effect).

     Come from [heft-lang\/POPL2023\/haskell\/src\/Hefty.hs]
    (https://github.com/heft-lang/POPL2023/blob/74afe1d5ce0b491cffe40cc5c73a2a5ee6a94d9c/haskell/src/Hefty.hs#L9-L10).
-}
class HFunctor h where
    -- | Hoist the monad underlying a /signature/.
    hmap :: (forall x. f x -> g x) -> h f a -> h g a

{- | Lift an /instruction/ (a datatype of first-order effect) to a /signature/
    (a datatype of higher-order effect).

     Come from [heft-lang\/POPL2023\/haskell\/src\/Elab.hs]
    (https://github.com/heft-lang/POPL2023/blob/74afe1d5ce0b491cffe40cc5c73a2a5ee6a94d9c/haskell/src/Elab.hs#L9-L10).
-}
newtype LiftIns ins (f :: Type -> Type) (a :: Type) = LiftIns {unliftIns :: ins a}
    deriving stock (Functor, Foldable, Traversable)

instance HFunctor (LiftIns ins) where
    hmap _ = LiftIns . unliftIns

type Signature = (Type -> Type) -> Type -> Type
