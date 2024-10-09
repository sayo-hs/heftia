{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}

-- SPDX-License-Identifier: MPL-2.0

module Main where

import Control.Arrow ((>>>))
import Control.Effect.Interpreter.Heftia.Provider (ProviderFix_, provide_, runProvider_)
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

data FileSystemF a where
    ReadFS :: FilePath -> FileSystemF String
    WriteFS :: FilePath -> String -> FileSystemF ()

data FileSystemH m (a :: Type) where
    TransactFS :: m a -> FileSystemH m a
makeEffect [''FileSystemF] [''FileSystemH]

type FSProvider eh ef = ProviderFix_ FilePath FileSystemH eh FileSystemF ef

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
        provide_ @"fs1" "/fs1" \_ -> do
            provide_ @"fs2" "/fs2" \inBase -> do
                inBase do
                    s1 <- readFS' @"fs1" "/a/b/c"
                    liftIO $ putStrLn $ "content: " <> show s1
                    writeFS' @"fs1" "/d/e/f" "foobar"

                liftIO $ putStrLn "-----"

                s2 <- readFS' @"fs2" "/a/b/c"
                liftIO $ putStrLn $ "content: " <> show s2
                writeFS' @"fs2" "/d/e/f" "foobar"

                liftIO $ putStrLn "-----"

                transactFS' @"fs2" do
                    inBase $ transactFS' @"fs1" do
                        liftIO $ print "hello"
