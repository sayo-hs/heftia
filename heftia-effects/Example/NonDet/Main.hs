{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

-- SPDX-License-Identifier: MPL-2.0

module Main where

import Control.Monad.Hefty (
    Eff,
    interpret,
    liftIO,
    makeEffectF,
    runEff,
    (&),
    type (<:),
    type (<|),
    type (~>),
 )
import Control.Monad.Hefty.Except (Throw, joinEither, runThrowIO, throw)
import Control.Monad.Hefty.NonDet (Choose, Empty, choice, runNonDetMonoid)
import Data.Function (fix)
import Data.Functor ((<&>))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Monoid (Sum (Sum))
import System.FilePath (splitDirectories, (</>))
import UnliftIO (Exception)

-- | Effect for file system operations
data FileSystem a where
    ListDirectory :: FilePath -> FileSystem (Either NotADir [FilePath])
    GetFileSize :: FilePath -> FileSystem (Either NotAFile Integer)

-- | Exception for when a directory was expected but found a file
data NotAFile = NotAFile

-- | Exception for when a file was expected but found a directory
data NotADir = NotADir
    deriving (Show)
    deriving anyclass (Exception)

makeEffectF [''FileSystem]

-- | Exception for when an entry does not exist at the specified path
data EntryNotFound = EntryNotFound
    deriving (Show)
    deriving anyclass (Exception)

-- | Aggregate the sizes of all files under the given path
totalFileSize
    :: (Choose <| ef, Empty <| ef, FileSystem <| ef, Throw NotADir <| ef, IO <| ef)
    => FilePath
    -> Eff '[] ef (Sum Integer)
totalFileSize path = do
    entities :: [FilePath] <- listDirectory path & joinEither
    entity :: FilePath <- choice entities -- Non-deterministically "pick" one item from the list
    let path' = path </> entity

    liftIO $ putStrLn $ "Found " <> path'

    getFileSize path' >>= \case
        Right size -> pure $ Sum size
        Left NotAFile -> do
            totalFileSize path'

main :: IO ()
main = runEff
    . runThrowIO @EntryNotFound
    . runThrowIO @NotADir
    . runDummyFS exampleRoot
    $ do
        total <- runNonDetMonoid pure (totalFileSize ".")
        liftIO $ print total

{-
>>> main
Found ./README.md
Found ./src
Found ./src/Bar.hs
Found ./src/Foo.hs
Found ./test
Found ./test/Baz.hs
Sum {getSum = 10000}
-}

-- | Example directory structure used this time
exampleRoot :: FSTree
exampleRoot =
    dir
        [
            ( "."
            , dir
                [
                    ( "src"
                    , dir
                        [ ("Bar.hs", File 1000)
                        , ("Foo.hs", File 2000)
                        ]
                    )
                ,
                    ( "test"
                    , dir
                        [ ("Baz.hs", File 3000)
                        ]
                    )
                , ("README.md", File 4000)
                ]
            )
        ]
  where
    dir :: [(FilePath, FSTree)] -> FSTree
    dir = Dir . Map.fromList

-- | Directory structure
data FSTree
    = Dir {entries :: Map FilePath FSTree}
    | File {fileSize :: Integer}

{- |
Interpreter for the FileSystem effect that virtualizes the file system in memory
based on a given FSTree, instead of performing actual IO.
-}
runDummyFS
    :: (Throw EntryNotFound <| ef, Throw NotADir <| ef)
    => FSTree
    -> Eff eh (FileSystem ': ef) ~> Eff eh ef
runDummyFS root = interpret \case
    ListDirectory path ->
        lookupFS path root <&> \case
            Dir entries -> Right $ Map.keys entries
            File _ -> Left NotADir
    GetFileSize path ->
        lookupFS path root <&> \case
            File size -> Right size
            Dir _ -> Left NotAFile

-- | Lookup the directory structure by path
lookupFS
    :: (Throw EntryNotFound <: m, Throw NotADir <: m, Monad m)
    => FilePath
    -> FSTree
    -> m FSTree
lookupFS path =
    splitDirectories path & fix \dive -> \case
        [] -> pure
        dirName : restPath -> \case
            Dir currentDir -> do
                case currentDir Map.!? dirName of
                    Just restTree -> dive restPath restTree
                    Nothing -> throw EntryNotFound
            File _ -> throw NotADir
