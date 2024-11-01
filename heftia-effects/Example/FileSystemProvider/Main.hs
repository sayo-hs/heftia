{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

-- SPDX-License-Identifier: MPL-2.0

module Main where

import Control.Arrow ((>>>))
import Control.Monad.Hefty (
    Eff,
    Type,
    interpret,
    interpretH,
    liftIO,
    makeEffect,
    runEff,
    type (<|),
    type (~>),
 )
import Control.Monad.Hefty.Provider (Provider_, runProvider_, scope_)

data FileSystemF a where
    ReadFS :: FilePath -> FileSystemF String
    WriteFS :: FilePath -> String -> FileSystemF ()

data FileSystemH m (a :: Type) where
    TransactFS :: m a -> FileSystemH m a
makeEffect [''FileSystemF] [''FileSystemH]

type FSProvider eh ef = Provider_ FilePath FileSystemH FileSystemF eh ef

runDummyFSProvider :: (IO <| ef) => Eff (FSProvider eh ef ': eh) ef ~> Eff eh ef
runDummyFSProvider =
    runProvider_ \workDir ->
        interpretH \case
            TransactFS m -> do
                liftIO $ putStrLn $ "[DUMMY FS " <> workDir <> "] START TRANSACTION"
                r <- m
                liftIO $ putStrLn $ "[DUMMY FS " <> workDir <> "] END TRANSACTION"
                pure r
            >>> interpret \case
                ReadFS path -> do
                    liftIO $ putStrLn $ "[DUMMY FS " <> workDir <> "] readFS " <> show path
                    pure $ "DUMMY CONTENT on " <> workDir <> path
                WriteFS path s -> do
                    liftIO $ putStrLn $ "[DUMMY FS " <> workDir <> "] writeFS " <> show path <> " " <> show s

main :: IO ()
main =
    runEff . runDummyFSProvider $
        scope_ @"fs1" "/fs1" \_ -> do
            scope_ @"fs2" "/fs2" \outer -> do
                outer do
                    s1 <- readFS' @"fs1" "/a/b/c"
                    liftIO $ putStrLn $ "content: " <> show s1
                    writeFS' @"fs1" "/d/e/f" "foobar"

                liftIO $ putStrLn "-----"

                s2 <- readFS' @"fs2" "/a/b/c"
                liftIO $ putStrLn $ "content: " <> show s2
                writeFS' @"fs2" "/d/e/f" "foobar"

                liftIO $ putStrLn "-----"

                transactFS' @"fs2" do
                    outer $ transactFS' @"fs1" do
                        liftIO $ print "hello"

{-
[DUMMY FS /fs1] readFS "/a/b/c"
content: "DUMMY CONTENT on /fs1/a/b/c"
[DUMMY FS /fs1] writeFS "/d/e/f" "foobar"
-----
[DUMMY FS /fs2] readFS "/a/b/c"
content: "DUMMY CONTENT on /fs2/a/b/c"
[DUMMY FS /fs2] writeFS "/d/e/f" "foobar"
-----
[DUMMY FS /fs2] START TRANSACTION
[DUMMY FS /fs1] START TRANSACTION
"hello"
[DUMMY FS /fs1] END TRANSACTION
[DUMMY FS /fs2] END TRANSACTION
-}
