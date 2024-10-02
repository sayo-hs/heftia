-- SPDX-License-Identifier: MPL-2.0

module Control.Monad.Hefty.Interpret.State where

import Control.Effect (type (~>))
import Control.Monad.Hefty.Interpret (qApp)
import Control.Monad.Hefty.Types (Eff (Op, Val), sendUnionBy, sendUnionHBy)
import Data.Effect.OpenUnion.Internal (IsSuffixOf)
import Data.Effect.OpenUnion.Internal.FO (Union, prj, weakens, (!+), type (<|))
import Data.Effect.OpenUnion.Internal.HO (HFunctors, UnionH, hfmapUnion, nilH)
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
    iterStateAllEffHFBy s0 ret nilH (hdl !+ \u s k -> sendUnionBy (k s) (weakens u))
{-# INLINE reinterpretStateBy #-}

interpretStateRecWith
    :: forall s e ef eh a
     . (HFunctors eh)
    => s
    -> (forall ans. StateInterpreter s e (Eff eh ef) ans)
    -> Eff eh (e ': ef) a
    -> Eff eh ef a
interpretStateRecWith = reinterpretStateRecWith
{-# INLINE interpretStateRecWith #-}

reinterpretStateRecWith
    :: forall s e ef' ef eh a
     . (ef `IsSuffixOf` ef', HFunctors eh)
    => s
    -> (forall ans. StateInterpreter s e (Eff eh ef') ans)
    -> Eff eh (e ': ef) a
    -> Eff eh ef' a
reinterpretStateRecWith s0 hdl = loop s0
  where
    loop :: s -> Eff eh (e ': ef) ~> Eff eh ef'
    loop s =
        iterStateAllEffHFBy
            s
            (const pure)
            (\u s' k -> sendUnionHBy (k s') $ hfmapUnion (loop s') u)
            (hdl !+ \u s' k -> sendUnionBy (k s') (weakens u))
{-# INLINE reinterpretStateRecWith #-}

interposeStateBy
    :: forall s e ef ans a
     . (e <| ef)
    => s
    -> (s -> a -> Eff '[] ef ans)
    -> StateInterpreter s e (Eff '[] ef) ans
    -> Eff '[] ef a
    -> Eff '[] ef ans
interposeStateBy s0 ret f =
    iterStateAllEffHFBy s0 ret nilH \u s k ->
        maybe (sendUnionBy (k s) u) (\e -> f e s k) (prj @e u)
{-# INLINE interposeStateBy #-}

-- TODO: add other pattern functions.
