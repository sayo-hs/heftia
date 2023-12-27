module Control.Effect.ExtensibleTree where

import Control.Effect.Free (EffectfulF)
import Control.Effect.Hefty (Effectful)
import Control.Monad.Freer.Tree (FreerTree)
import Data.Hefty.Extensible (ExtensibleUnion)

infixr 4 !!
infixr 3 !

type (!!) = Effectful ExtensibleUnion FreerTree
type (!) = EffectfulF ExtensibleUnion FreerTree
