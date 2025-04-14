-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.
{-# LANGUAGE AllowAmbiguousTypes #-}

module Control.Monad.Hefty.ShiftReset (
    module Control.Monad.Hefty.ShiftReset,
    module Data.Effect.ShiftReset,
)
where

import Control.Effect (emb)
import Control.Monad.Hefty (
    Catch,
    Eff,
    interpose,
    interpret,
    interpretBy,
    raise,
    runEff,
 )
import Control.Monad.Hefty.Types (Freer)
import Data.Effect (Catch (Catch), Emb, Throw (Throw))
import Data.Effect.Except (throw)
import Data.Effect.OpenUnion (FOEs, (:>))
import Data.Effect.ShiftReset hiding (Shift)
import Data.Effect.ShiftReset qualified as D
import Data.Function ((&))
import Data.Void (Void, absurd)

type Shift ans es = D.Shift Freer ans es

evalShift :: (FOEs es) => Eff (Shift ans es ': es) ans -> Eff es ans
evalShift = runShift pure

runShift :: (FOEs es) => (a -> Eff es ans) -> Eff (Shift ans es ': es) a -> Eff es ans
runShift f =
    interpretBy f \e k ->
        evalShift $ case e of
            UnliftShift initiate -> unShiftC $ initiate (ShiftC . raise . k) ShiftC

runShiftExit :: (Monad m) => Eff '[Shift Void '[Throw a, Emb m], Throw a, Emb m] a -> m a
runShiftExit m = do
    runEff $ runThrowExit $ runShift throw m

runThrowExit :: (FOEs es) => Eff (Throw a ': es) Void -> Eff es a
runThrowExit m = m & interpretBy absurd \(Throw x) _ -> pure x

withShift :: Eff '[Shift ans '[Emb (Eff es)], Emb (Eff es)] ans -> Eff es ans
withShift = runEff . evalShift

runReset :: forall es a. Eff (Reset ': es) a -> Eff es a
runReset = interpret \(Reset a) -> a
{-# INLINE runReset #-}

runThrow :: (Shift Void es' :> es) => Eff (Throw e ': es) a -> Eff es (Either e a)
runThrow m =
    callCC \exit ->
        Right <$> do
            m & interpret \case
                Throw e -> absurd <$> exit (Left e)

runCatch
    :: forall e es es' a
     . (Shift Void es' :> es, Throw e :> es)
    => Eff (Catch e ': es) a
    -> Eff es a
runCatch m =
    m & interpret \case
        Catch a hdl ->
            callCC @_ @Void @_ @es' \exit -> do
                a & interpose @(Throw e) \case
                    Throw e -> fmap absurd . exit =<< hdl e

runUnliftShift
    :: forall es' es a
     . (Shift Void es' :> es)
    => Eff '[Shift Void '[Emb (Eff es)], Emb (Eff es)] a
    -> Eff es a
runUnliftShift =
    runEff . interpret \case
        UnliftShift initiate -> emb @(Eff es) $
            callCC \exit -> do
                fmap absurd $
                    runUnliftShift $
                        unShiftC $
                            initiate
                                (ShiftC . emb @(Eff es) . exit)
                                (ShiftC . raise)
