{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}

-- SPDX-License-Identifier: MPL-2.0

module Main where

import Control.Monad.Hefty (
    Eff,
    Effect,
    FOEs,
    interposeBy,
    interpret,
    liftIO,
    makeEffectF,
    makeEffectH,
    runEff,
    (&),
    (:>),
    type (~>),
    type (~~>),
 )

type ForkID = Int

data Fork :: Effect where
    Fork :: Fork f ForkID
makeEffectF ''Fork

runForkSingle :: Eff (Fork ': es) ~> Eff es
runForkSingle = interpret \Fork -> pure 0

data ResetFork f a where
    ResetFork :: (Monoid w) => f w -> ResetFork f w
makeEffectH ''ResetFork

applyResetFork :: (Fork :> es, FOEs es) => Int -> ResetFork ~~> Eff es
applyResetFork numberOfFork (ResetFork m) =
    m & interposeBy pure \Fork resume -> do
        r <- mapM resume [1 .. numberOfFork]
        pure $ mconcat r

main :: IO ()
main =
    runEff
        . runForkSingle
        . interpret (applyResetFork 4)
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
