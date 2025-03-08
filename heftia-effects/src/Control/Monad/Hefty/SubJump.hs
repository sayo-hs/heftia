{-# LANGUAGE AllowAmbiguousTypes #-}

-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2024 Sayo contributors
License     :  MPL-2.0 (see the LICENSE file)
Maintainer  :  ymdfield@outlook.jp
-}
module Control.Monad.Hefty.SubJump (
    module Control.Monad.Hefty.SubJump,
    module Data.Effect.SubJump,
)
where

import Control.Arrow ((>>>))
import Control.Effect (emb)
import Control.Effect.Interpret (interpose, runEff)
import Control.Monad.Hefty (Eff, MemberBy, interpret, interpretBy, (&))
import Control.Monad.Hefty.ShiftReset (runThrowExit)
import Data.Effect (Catch, Emb)
import Data.Effect.Except (Catch (Catch), Throw (Throw), throw)
import Data.Effect.OpenUnion (FOEs, (:>))
import Data.Effect.SubJump
import Data.Functor.Contravariant qualified as C
import Data.Void (Void, absurd)

runSubJump :: (FOEs es) => (a -> Eff es ans) -> Eff (SubJump (C.Op (Eff es ans)) ': es) a -> Eff es ans
runSubJump k =
    interpretBy k \case
        SubFork -> \exit -> exit . Left . C.Op $ exit . Right
        Jump (C.Op exit) x -> \_ -> exit x

evalSubJump :: (FOEs es) => Eff (SubJump (C.Op (Eff es a)) ': es) a -> Eff es a
evalSubJump = runSubJump pure

throwToSubJump
    :: (SubJump ref :> es)
    => Eff (Throw e ': es) a
    -> Eff es (Either e a)
throwToSubJump m =
    callCC_ \exit -> Right <$> m & interpret \(Throw e) -> absurd <$> exit (Left e)

catchToSubJump :: forall ref e es a. (SubJump ref :> es, Throw e :> es) => Eff (Catch e ': es) a -> Eff es a
catchToSubJump m =
    m & interpret \(Catch thing hdl) ->
        callCC_ @ref \exit ->
            thing & interpose @(Throw e) \(Throw e) -> fmap absurd . exit =<< hdl e

runSubJumpExit :: (Monad m) => Eff '[SubJump (C.Op (Eff '[Throw a, Emb m] Void)), Throw a, Emb m] a -> m a
runSubJumpExit = runEff . runThrowExit . runSubJump throw

runUnliftSubJump :: forall es' es a ref. (SubJump ref :> es', Emb (Eff es') :> es) => Eff (SubJump ref ': es) a -> Eff es a
runUnliftSubJump = interpret \case
    SubFork -> emb subFork
    Jump ref x -> emb $ jump ref x
