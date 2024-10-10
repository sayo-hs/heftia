{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}

-- SPDX-License-Identifier: MPL-2.0

module Main where

import Control.Applicative ((<|>))
import Control.Arrow ((>>>))
import Control.Monad.Hefty (
    Eff,
    Type,
    interpret,
    interpretH,
    liftIO,
    makeEffectF,
    makeEffectH,
    raiseNH,
    type (<<|),
    type (<|),
    type (~>),
 )
import Control.Monad.Hefty.NonDet (runChooseH, runNonDetMonoid)
import Control.Monad.Hefty.Resource (runResourceIO)
import Control.Monad.Hefty.Unlift (runUnliftIO)
import Data.Effect.Resource (Resource, bracket_)

data DBF (a :: Type) where
    InsertDB :: Int -> DBF ()
makeEffectF [''DBF]

data DBH m (a :: Type) where
    Transact :: m a -> DBH m a
makeEffectH [''DBH]

runDummyDB :: (Resource <<| eh, IO <| ef) => Eff (DBH ': eh) (DBF ': ef) ~> Eff eh ef
runDummyDB =
    interpretH \case
        Transact m ->
            bracket_
                (liftIO $ putStrLn "[DummyDB] Start transaction.")
                (liftIO $ putStrLn "[DummyDB] End transaction.")
                m
        >>> interpret \case
            InsertDB x -> liftIO $ putStrLn $ "[DummyDB] insertDB " <> show x

main :: IO ()
main =
    runUnliftIO . runResourceIO . runDummyDB $ do
        insertDB 42

        transact do
            insertDB 123
            insertDB 456

            -- Even within the scope of UnliftIO, you can combine
            -- non-deterministic computations (or even coroutines).
            raiseNH @3
                . runNonDetMonoid (const $ pure ())
                . runChooseH
                $ do
                    insertDB 1 <|> insertDB 2

        insertDB 789

{-
[DummyDB] insertDB 42
[DummyDB] Start transaction.
[DummyDB] insertDB 123
[DummyDB] insertDB 456
[DummyDB] insertDB 1
[DummyDB] insertDB 2
[DummyDB] End transaction.
[DummyDB] insertDB 789
-}
