{-# LANGUAGE TemplateHaskell #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Main where

import Control.Effect.Class (type (~>))
import Control.Effect.Class.Embed (Embed, EmbedI, embed)
import Control.Effect.Class.Except (ThrowI, throw)
import Control.Effect.Class.Machinery.HFunctor (HFunctor)
import Control.Effect.Class.Machinery.TH (makeEffectF, makeEffectH)
import Control.Effect.Freer (Fre, interposeK, interpret, type (<:))
import Control.Effect.Handler.Heftia.Embed (runEmbed)
import Control.Effect.Heftia (Hef, hoistHeftiaEffects, interposeH, interpretH, liftLowerH, type (<<:))
import Data.Hefty.Sum (SumH)
import Data.Word (Word8)

type URL = String
type ByteString = [Word8]

class ImageURL f where
    getImageURL :: f URL
makeEffectF ''ImageURL

class FetchImageProc f where
    fetchImageProc :: f a -> f a
makeEffectH ''FetchImageProc

class FetchURL f where
    fetchURL :: URL -> f ByteString
makeEffectF ''FetchURL

runDummyFetchURL :: (EmbedI IO <: r, Monad m) => Fre (FetchURLI ': r) m ~> Fre r m
runDummyFetchURL = interpret \case
    FetchURL uri -> do
        embed $ putStrLn $ "<runDummyFetchURL> " <> uri
        pure [0xBE, 0xEF] -- dummy data

class FileSystem f where
    writeFS :: FilePath -> ByteString -> f ()

makeEffectF ''FileSystem

runDummyFS :: (Embed IO (Fre r m), Monad m) => Fre (FileSystemI ': r) m ~> Fre r m
runDummyFS = interpret \case
    WriteFS path content -> embed $ putStrLn $ "<runDummyFS> writeFS " <> path <> " : " <> show content

downloadImage :: (ImageURL m, FetchURL m, FileSystem m, FetchImageProc m, Monad m) => m ()
downloadImage = do
    imageData <- fetchImageProc do
        imageURL <- getImageURL
        fetchURL imageURL
    writeFS "image.dat" imageData

passthroughFetchImageProc :: (HFunctor (SumH r), Monad m) => Hef (FetchImageProcS ': r) m ~> Hef r m
passthroughFetchImageProc = interpretH \(FetchImageProc m) -> m

tryFetchForCandidateImageURLs ::
    (ImageURLI <: es', FetchImageProcS <<: es, HFunctor (SumH es), ThrowI FetchFailed <: es', Monad m) =>
    [URL] ->
    Hef es (Fre es' m) ~> Hef es (Fre es' m)
tryFetchForCandidateImageURLs candidates =
    interposeH \(FetchImageProc m) ->
        fetchImageProc do
            ($ m) $ hoistHeftiaEffects $ interposeK pure \k GetImageURL -> do
                let go = \case
                        [] -> throw FetchFailed
                        x : xs -> do
                            r <- k x
                            undefined
                go candidates

data FetchFailed = FetchFailed

main :: IO ()
main = runEmbed
    . runDummyFetchURL
    $ do
        undefined
