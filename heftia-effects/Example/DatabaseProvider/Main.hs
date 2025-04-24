{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}

-- SPDX-License-Identifier: MPL-2.0

module Main where

import Control.Monad.Hefty (
    Eff,
    Effect,
    Emb,
    Freer,
    RemoveExps,
    UnliftIO,
    interprets,
    liftIO,
    makeEffectF,
    makeEffectH,
    nil,
    (!:),
    type (:>),
    type (~>),
 )
import Control.Monad.Hefty.Provider (Provider, provide_, runRegionProvider_)
import Control.Monad.Hefty.Unlift (runUnliftIO)
import Data.Effect.OpenUnion (WeakenExps)
import Data.Functor.Identity (Identity)

data DatabaseF :: Effect where
    ReadDB :: FilePath -> DatabaseF f String
    WriteDB :: FilePath -> String -> DatabaseF f ()

data DatabaseH :: Effect where
    TransactDB :: m a -> DatabaseH m a
makeEffectF ''DatabaseF
makeEffectH ''DatabaseH

type DBProvider es = Provider Freer Identity FilePath '[DatabaseH, DatabaseF] es

runDummyDBProvider
    :: (UnliftIO :> es, Emb IO :> es, Emb IO :> RemoveExps es, WeakenExps es)
    => Eff (DBProvider (RemoveExps es) ': es) ~> Eff es
runDummyDBProvider =
    runRegionProvider_ @IO \workDir ->
        interprets $
            ( \(TransactDB m) -> do
                liftIO $ putStrLn $ "[DUMMY DB " <> workDir <> "] START TRANSACTION"
                r <- m
                liftIO $ putStrLn $ "[DUMMY DB " <> workDir <> "] END TRANSACTION"
                pure r
            )
                !: ( \case
                        ReadDB path -> do
                            liftIO $ putStrLn $ "[DUMMY DB " <> workDir <> "] readDB " <> show path
                            pure $ "DUMMY CONTENT on " <> workDir <> path
                        WriteDB path s -> do
                            liftIO $ putStrLn $ "[DUMMY DB " <> workDir <> "] writeDB " <> show path <> " " <> show s
                   )
                !: nil

main :: IO ()
main =
    runUnliftIO . runDummyDBProvider $
        provide_ "/db1" \_ -> do
            provide_ "/db2" \detach -> do
                detach do
                    s1 <- readDB "/a/b/c"
                    liftIO $ putStrLn $ "content: " <> show s1
                    writeDB "/d/e/f" "foobar"

                liftIO $ putStrLn "-----"

                s2 <- readDB "/a/b/c"
                liftIO $ putStrLn $ "content: " <> show s2
                writeDB "/d/e/f" "foobar"

                liftIO $ putStrLn "-----"

                transactDB do
                    detach $ transactDB do
                        liftIO $ print "hello"

{-
[DUMMY DB /db1] readDB "/a/b/c"
content: "DUMMY CONTENT on /db1/a/b/c"
[DUMMY DB /db1] writeDB "/d/e/f" "foobar"
-----
[DUMMY DB /db2] readDB "/a/b/c"
content: "DUMMY CONTENT on /db2/a/b/c"
[DUMMY DB /db2] writeDB "/d/e/f" "foobar"
-----
[DUMMY DB /db2] START TRANSACTION
[DUMMY DB /db1] START TRANSACTION
"hello"
[DUMMY DB /db1] END TRANSACTION
[DUMMY DB /db2] END TRANSACTION
-}
