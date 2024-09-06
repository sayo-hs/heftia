{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Main where

import Control.Effect (type (~>))
import Control.Effect.ExtensibleChurch (runEff, type (:!!))
import Control.Effect.Hefty (Elab, interposeK, interpretRec, interpretRecH)
import Control.Monad.IO.Class (liftIO)
import Data.Effect.TH (makeEffectF, makeEffectH)
import Data.Function ((&))
import Data.Hefty.Extensible (ForallHFunctor, type (<|))

type ForkID = Int

data Fork a where
    Fork :: Fork ForkID
makeEffectF [''Fork]

runForkSingle :: ForallHFunctor eh => eh :!! LFork ': r ~> eh :!! r
runForkSingle = interpretRec \Fork -> pure 0

data ResetFork f a where
    ResetFork :: Monoid w => f w -> ResetFork f w
makeEffectH [''ResetFork]

applyResetFork :: Fork <| r => Int -> Elab ResetFork ('[] :!! r)
applyResetFork numberOfFork (ResetFork m) =
    m & interposeK pure \resume Fork -> do
        r <- mapM resume [1 .. numberOfFork]
        pure $ mconcat r

main :: IO ()
main =
    runEff
        . runForkSingle
        . interpretRecH (applyResetFork 4)
        $ do
            liftIO . putStrLn . (("[out of scope] " ++) . show) =<< fork
            s <- resetFork do
                fid1 <- fork
                fid2 <- fork
                liftIO $ putStrLn $ "[delimited continuation of `fork`] Fork ID: " ++ show (fid1, fid2)
                pure $ show (fid1, fid2)
            liftIO $ putStrLn $ "scope exited. result: " ++ s

{-
[out of scope] 0
[delimited continuation of `fork`] Fork ID: (1,1)
[delimited continuation of `fork`] Fork ID: (1,2)
[delimited continuation of `fork`] Fork ID: (1,3)
[delimited continuation of `fork`] Fork ID: (1,4)
[delimited continuation of `fork`] Fork ID: (2,1)
[delimited continuation of `fork`] Fork ID: (2,2)
[delimited continuation of `fork`] Fork ID: (2,3)
[delimited continuation of `fork`] Fork ID: (2,4)
[delimited continuation of `fork`] Fork ID: (3,1)
[delimited continuation of `fork`] Fork ID: (3,2)
[delimited continuation of `fork`] Fork ID: (3,3)
[delimited continuation of `fork`] Fork ID: (3,4)
[delimited continuation of `fork`] Fork ID: (4,1)
[delimited continuation of `fork`] Fork ID: (4,2)
[delimited continuation of `fork`] Fork ID: (4,3)
[delimited continuation of `fork`] Fork ID: (4,4)
scope exited. result: (1,1)(1,2)(1,3)(1,4)(2,1)(2,2)(2,3)(2,4)(3,1)(3,2)(3,3)(3,4)(4,1)(4,2)(4,3)(4,4)
-}
