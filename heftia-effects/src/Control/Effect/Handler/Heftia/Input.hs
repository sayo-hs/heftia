-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2024 Yamada Ryo
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable
-}
module Control.Effect.Handler.Heftia.Input where
import Control.Freer (Freer)
import Data.Hefty.Union (Union (HasMembership))
import Control.Effect.Hefty (Eff, interpret, interpretRec, raiseUnder)
import Data.Effect.HFunctor (HFunctor)
import Data.Effect.Input (Input(Input), LInput)
import Control.Effect (type (~>))
import Control.Arrow ((>>>))
import Control.Effect.Handler.Heftia.State (evalState)
import Control.Monad.State (StateT)
import Data.Effect.State (LState, gets, put)
import Data.List (uncons)

runInputEff ::
    forall i r eh fr u c.
    (Freer c fr, Union u, Applicative (Eff u fr eh r), HFunctor (u eh)) =>
    Eff u fr eh r i -> Eff u fr eh (LInput i ': r) ~> Eff u fr eh r
runInputEff a = interpretRec \Input -> a
{-# INLINE runInputEff #-}

runInputConst ::
    forall i r eh fr u c.
    (Freer c fr, Union u, Applicative (Eff u fr eh r), HFunctor (u eh)) =>
    i -> Eff u fr eh (LInput i ': r) ~> Eff u fr eh r
runInputConst i = interpretRec \Input -> pure i
{-# INLINE runInputConst #-}

runInputList ::
    forall i r fr u c.
    ( Freer c fr, Union u
    , Applicative (Eff u fr '[] r), Monad (Eff u fr '[] (LState [i] ': r))
    , c (Eff u fr '[] r), c (StateT [i] (Eff u fr '[] r))
    , HasMembership u (LState [i]) (LState [i] ': r)
    , HFunctor (u '[])
    ) =>
    [i] -> Eff u fr '[] (LInput (Maybe i) ': r) ~> Eff u fr '[] r
runInputList is =
        raiseUnder
    >>> ( interpret \Input -> do
            is' <- gets @[i] uncons
            mapM_ (put . snd) is'
            pure $ fst <$> is'
        )
    >>> evalState is
