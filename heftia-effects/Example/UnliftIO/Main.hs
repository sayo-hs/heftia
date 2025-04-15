{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}

-- SPDX-License-Identifier: MPL-2.0

module Main where

import Control.Applicative ((<|>))
import Control.Monad.Hefty (
    Eff,
    Effect,
    Emb,
    Type,
    UnliftIO,
    interprets,
    liftIO,
    makeEffectF,
    makeEffectH,
    nil,
    onlyFOEs,
    (!:),
    type (:>),
    type (~>),
 )
import Control.Monad.Hefty.Coroutine (Status (Continue, Done), runCoroutine, yield)
import Control.Monad.Hefty.NonDet (runChooseH, runNonDetMonoid)
import Control.Monad.Hefty.Unlift (runUnliftIO)
import UnliftIO (bracket_)

data DBF :: Effect where
    InsertDB :: Int -> DBF f ()
makeEffectF ''DBF

data DBH m (a :: Type) where
    Transact :: m a -> DBH m a
makeEffectH ''DBH

runDummyDB :: (UnliftIO :> es, Emb IO :> es) => Eff (DBH ': DBF ': es) ~> Eff es
runDummyDB =
    interprets $
        ( \(Transact m) ->
            bracket_
                (liftIO $ putStrLn "[DummyDB] Start transaction.")
                (liftIO $ putStrLn "[DummyDB] End transaction.")
                m
        )
            !: (\(InsertDB x) -> liftIO $ putStrLn $ "[DummyDB] insertDB " <> show x)
            !: nil

main :: IO ()
main =
    runUnliftIO . runDummyDB $ do
        insertDB 42

        transact do
            insertDB 123
            insertDB 456

            onlyFOEs do
                -- Even within the scope of UnliftIO, you can combine
                -- non-deterministic computations...
                runNonDetMonoid (const $ pure ()) . runChooseH $ do
                    insertDB 1 <|> insertDB 2

                -- ...or even use coroutines.
                do
                    status <- runCoroutine do
                        yield $ -10
                        insertDB $ -20

                    case status of
                        Done () -> pure ()
                        Continue x resume -> do
                            _ <- resume ()
                            insertDB x
                            pure ()

                -- Note that UnliftIO is disabled within the scope of 'onlyFOEs'.
                -- Fitst-order 'IO' operations are still possible.
                liftIO $ putStrLn "The transaction is being finalized..."

        insertDB 789

{-
[DummyDB] insertDB 42
[DummyDB] Start transaction.
[DummyDB] insertDB 123
[DummyDB] insertDB 456
[DummyDB] insertDB 1
[DummyDB] insertDB 2
[DummyDB] insertDB -20
[DummyDB] insertDB -10
The transaction is being finalized...
[DummyDB] End transaction.
[DummyDB] insertDB 789
-}
