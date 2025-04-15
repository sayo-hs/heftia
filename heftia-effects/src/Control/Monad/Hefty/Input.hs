-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2024 Sayo contributors
License     :  MPL-2.0 (see the LICENSE file)
Maintainer  :  ymdfield@outlook.jp

Interpreters for the t'Input' effect.
-}
module Control.Monad.Hefty.Input (
    module Control.Monad.Hefty.Input,
    module Data.Effect.Input,
)
where

import Control.Arrow ((>>>))
import Control.Monad.Hefty (CC, Eff, interpret, raiseUnder, (:>))
import Data.Effect.Input
import Data.Effect.State (evalStateCC, gets, put)
import Data.List (uncons)

{- |
Interprets the t'Input' effect by using the given list as a series of inputs.

Each time 'input' is called, it retrieves elements from the list one by one from the beginning, and after all elements are consumed, 'Nothing' is returned indefinitely.
-}
runInputList :: forall i a es ref. (CC ref :> es) => [i] -> Eff (Input (Maybe i) ': es) a -> Eff es a
runInputList is =
    raiseUnder
        >>> int
        >>> evalStateCC is
  where
    int = interpret \Input -> do
        is' <- gets @[i] uncons
        mapM_ (put . snd) is'
        pure $ fst <$> is'
{-# INLINE runInputList #-}
