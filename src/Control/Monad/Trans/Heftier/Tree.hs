module Control.Monad.Trans.Heftier.Tree where

import Data.Functor.Coyoneda (Coyoneda)

newtype HCoyoneda h f a = HCoyoneda {unHCoyoneda :: Coyoneda (h f) a}
