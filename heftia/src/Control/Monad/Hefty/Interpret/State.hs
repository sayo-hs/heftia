-- SPDX-License-Identifier: MPL-2.0

module Control.Monad.Hefty.Interpret.State where

import Control.Monad.Hefty.Interpret (qApp)
import Control.Monad.Hefty.Types (Eff (Op, Val), sendUnionBy)
import Data.Effect.OpenUnion.Internal (IsSuffixOf)
import Data.Effect.OpenUnion.Internal.FO (Union, weakens, (!+))
import Data.Effect.OpenUnion.Internal.HO (UnionH, nilH)
import Data.Kind (Type)

type StateInterpreter s e m (ans :: Type) = forall x. e x -> s -> (s -> x -> m ans) -> m ans

type StateElaborator s e m ans = StateInterpreter s (e m) m ans

iterStateAllEffHFBy
    :: forall s eh ef m ans a
     . (Monad m)
    => s
    -> (s -> a -> m ans)
    -> StateInterpreter s (UnionH eh (Eff eh ef)) m ans
    -> StateInterpreter s (Union ef) m ans
    -> Eff eh ef a
    -> m ans
iterStateAllEffHFBy s0 ret fh ff = loop s0
  where
    loop s = \case
        Val x -> ret s x
        Op u q -> either (`fh` s) (`ff` s) u k
          where
            k s' = loop s' . qApp q
{-# INLINE iterStateAllEffHFBy #-}

interpretStateBy
    :: forall s e ef ans a
     . s
    -> (s -> a -> Eff '[] ef ans)
    -> StateInterpreter s e (Eff '[] ef) ans
    -> Eff '[] (e ': ef) a
    -> Eff '[] ef ans
interpretStateBy = reinterpretStateBy
{-# INLINE interpretStateBy #-}

reinterpretStateBy
    :: forall s e ef' ef ans a
     . (ef `IsSuffixOf` ef')
    => s
    -> (s -> a -> Eff '[] ef' ans)
    -> StateInterpreter s e (Eff '[] ef') ans
    -> Eff '[] (e ': ef) a
    -> Eff '[] ef' ans
reinterpretStateBy s0 ret hdl =
    iterStateAllEffHFBy s0 ret nilH (hdl !+ \e s k -> sendUnionBy (k s) (weakens e))
{-# INLINE reinterpretStateBy #-}

-- TODO: add other pattern functions.
