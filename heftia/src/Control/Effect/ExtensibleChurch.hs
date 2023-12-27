module Control.Effect.ExtensibleChurch where

import Control.Effect.Free (EffectfulF)
import Control.Effect.Hefty (Effectful)
import Control.Monad.Freer.Church (FreerChurch)
import Data.Hefty.Extensible (ExtensibleUnion)

infixr 4 !!
infixr 3 !

type (!!) = Effectful ExtensibleUnion FreerChurch
type (!) = EffectfulF ExtensibleUnion FreerChurch
