{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- SPDX-License-Identifier: BSD-3-Clause
-- (c) 2021-2022, Andrzej Rybczak; 2024 Sayo contributors

module BenchFileSizes where

import Control.Exception (IOException, try)
import Control.Monad.IO.Class (MonadIO (..))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Data.Text qualified as T
import System.Posix (fileSize, getFileStatus)

-- effectful
import Effectful qualified as E
import Effectful.Dispatch.Dynamic qualified as E
import Effectful.Reader.Static qualified as E
import Effectful.State.Static.Local qualified as E

-- eff
#ifdef VERSION_eff
import "eff" Control.Effect qualified as L
#endif

-- cleff
#ifdef VERSION_cleff
import Cleff qualified as C
import Cleff.Reader qualified as C
import Cleff.State qualified as C
#endif

-- freer-simple
#ifdef VERSION_freer_simple
import Control.Monad.Freer qualified as FS
import Control.Monad.Freer.Reader qualified as FS
import Control.Monad.Freer.State qualified as FS
#endif

-- fused-effects
#ifdef VERSION_fused_effects
import Control.Algebra qualified as FE
import Control.Effect.Sum qualified as FE
import Control.Carrier.Reader qualified as FE
import Control.Carrier.State.Strict qualified as FE
#endif

-- mtl
#ifdef VERSION_mtl
import Control.Monad.State qualified as M
import Control.Monad.Reader qualified as M
#endif

-- polysemy
#ifdef VERSION_polysemy
import Polysemy qualified as P
import Polysemy.Reader qualified as P
import Polysemy.State qualified as P
#endif

-- heftia
import Control.Monad.Hefty qualified as H
import Control.Monad.Hefty.Reader qualified as H
import Control.Monad.Hefty.State qualified as H

tryGetFileSize :: FilePath -> IO (Maybe Int)
tryGetFileSize path =
    try @IOException (getFileStatus path) >>= \case
        Left _ -> pure Nothing
        Right stat -> pure . Just . fromIntegral $ fileSize stat

----------------------------------------
-- reference

ref_calculateFileSize :: IORef [Text] -> FilePath -> IO Int
ref_calculateFileSize logs path = do
    logToIORef logs $ "Calculating the size of " ++ path
    tryGetFileSize path >>= \case
        Nothing -> 0 <$ logToIORef logs ("Could not calculate the size of " ++ path)
        Just size -> size <$ logToIORef logs (path ++ " is " ++ show size ++ " bytes")
  where
    logToIORef :: IORef [Text] -> String -> IO ()
    logToIORef r msgS = do
        let msg = T.pack msgS
        msg `seq` modifyIORef' r (msg :)
{-# NOINLINE ref_calculateFileSize #-}

ref_program :: IORef [Text] -> [FilePath] -> IO Int
ref_program logs files = do
    sizes <- traverse (ref_calculateFileSize logs) files
    pure $ sum sizes
{-# NOINLINE ref_program #-}

ref_calculateFileSizes :: [FilePath] -> IO (Int, [Text])
ref_calculateFileSizes files = do
    logs <- newIORef []
    size <- ref_program logs files
    finalLogs <- readIORef logs
    pure (size, finalLogs)

----------------------------------------
-- heftia

data Heftia_File :: H.Effect where
    Heftia_tryFileSize :: FilePath -> Heftia_File f (Maybe Int)
H.makeEffectF ''Heftia_File

heftia_runFile :: (H.Emb IO H.:> es) => H.Eff (Heftia_File : es) a -> H.Eff es a
heftia_runFile = H.interpret \case
    Heftia_tryFileSize path -> liftIO $ tryGetFileSize path

data Heftia_Logging :: H.Effect where
    Heftia_logMsg :: !Text -> Heftia_Logging f ()
H.makeEffectF ''Heftia_Logging

heftia_runLogging :: (H.FOEs es) => H.Eff (Heftia_Logging : es) a -> H.Eff es ([Text], a)
heftia_runLogging =
    H.runState [] . H.reinterpret \case
        Heftia_logMsg msg -> H.modify (msg :)

----------

heftia_calculateFileSize
    :: (Heftia_File H.:> es, Heftia_Logging H.:> es)
    => FilePath
    -> H.Eff es Int
heftia_calculateFileSize path = do
    heftia_logMsg $ T.pack $ "Calculating the size of " ++ path
    heftia_tryFileSize path >>= \case
        Nothing -> 0 <$ heftia_logMsg (T.pack $ "Could not calculate the size of " ++ path)
        Just size -> size <$ heftia_logMsg (T.pack $ path ++ " is " ++ show size ++ " bytes")
{-# NOINLINE heftia_calculateFileSize #-}

heftia_program
    :: (Heftia_File H.:> es, Heftia_Logging H.:> es)
    => [FilePath]
    -> H.Eff es Int
heftia_program files = do
    sizes <- traverse heftia_calculateFileSize files
    pure $ sum sizes
{-# NOINLINE heftia_program #-}

heftia_calculateFileSizes :: [FilePath] -> IO ([Text], Int)
heftia_calculateFileSizes =
    H.runEff . heftia_runFile . heftia_runLogging . heftia_program

heftia_calculateFileSizesDeep :: [FilePath] -> IO ([Text], Int)
heftia_calculateFileSizesDeep =
    H.runEff
        . runR
        . runR
        . runR
        . runR
        . runR
        . heftia_runFile
        . heftia_runLogging
        . runR
        . runR
        . runR
        . runR
        . runR
        . heftia_program
  where
    runR = H.runAsk ()

----------------------------------------
-- effectful

data Effectful_File :: E.Effect where
    Effectful_tryFileSize :: FilePath -> Effectful_File m (Maybe Int)

type instance E.DispatchOf Effectful_File = 'E.Dynamic

effectful_tryFileSize :: (Effectful_File E.:> es) => FilePath -> E.Eff es (Maybe Int)
effectful_tryFileSize = E.send . Effectful_tryFileSize

effectful_runFile :: (E.IOE E.:> es) => E.Eff (Effectful_File : es) a -> E.Eff es a
effectful_runFile = E.interpret_ \case
    Effectful_tryFileSize path -> liftIO $ tryGetFileSize path

data Effectful_Logging :: E.Effect where
    Effectful_logMsg :: !Text -> Effectful_Logging m ()

type instance E.DispatchOf Effectful_Logging = 'E.Dynamic

effectful_logMsg :: (Effectful_Logging E.:> es) => String -> E.Eff es ()
effectful_logMsg = E.send . Effectful_logMsg . T.pack

effectful_runLogging
    :: E.Eff (Effectful_Logging : es) a
    -> E.Eff es (a, [Text])
effectful_runLogging = E.reinterpret_ (E.runState []) \case
    Effectful_logMsg msg -> E.modify (msg :)

----------

effectful_calculateFileSize
    :: (Effectful_File E.:> es, Effectful_Logging E.:> es)
    => FilePath
    -> E.Eff es Int
effectful_calculateFileSize path = do
    effectful_logMsg $ "Calculating the size of " ++ path
    effectful_tryFileSize path >>= \case
        Nothing -> 0 <$ effectful_logMsg ("Could not calculate the size of " ++ path)
        Just size -> size <$ effectful_logMsg (path ++ " is " ++ show size ++ " bytes")
{-# NOINLINE effectful_calculateFileSize #-}

effectful_program
    :: (Effectful_File E.:> es, Effectful_Logging E.:> es)
    => [FilePath]
    -> E.Eff es Int
effectful_program files = do
    sizes <- traverse effectful_calculateFileSize files
    pure $ sum sizes
{-# NOINLINE effectful_program #-}

effectful_calculateFileSizes :: [FilePath] -> IO (Int, [Text])
effectful_calculateFileSizes =
    E.runEff . effectful_runFile . effectful_runLogging . effectful_program

effectful_calculateFileSizesDeep :: [FilePath] -> IO (Int, [Text])
effectful_calculateFileSizesDeep =
    E.runEff
        . runR
        . runR
        . runR
        . runR
        . runR
        . effectful_runFile
        . effectful_runLogging
        . runR
        . runR
        . runR
        . runR
        . runR
        . effectful_program
  where
    runR = E.runReader ()

----------------------------------------
-- eff

#ifdef VERSION_eff

data Eff_File :: L.Effect where
  Eff_tryFileSize :: FilePath -> Eff_File m (Maybe Int)

eff_tryFileSize :: Eff_File L.:< es => FilePath -> L.Eff es (Maybe Int)
eff_tryFileSize = L.send . Eff_tryFileSize

eff_runFile :: L.IOE L.:< es => L.Eff (Eff_File : es) a -> L.Eff es a
eff_runFile = L.interpret \case
  Eff_tryFileSize path -> liftIO $ tryGetFileSize path

data Eff_Logging :: L.Effect where
  Eff_logMsg :: !Text -> Eff_Logging m ()

eff_logMsg :: Eff_Logging L.:< es => String -> L.Eff es ()
eff_logMsg = L.send . Eff_logMsg . T.pack

eff_runLogging
  :: L.Eff (Eff_Logging : es) a
  -> L.Eff es ([Text], a)
eff_runLogging
  = L.runState [] . L.interpret \case
      Eff_logMsg msg -> L.modify (msg :)
  . L.lift

----------

eff_calculateFileSize
  :: (Eff_File L.:< es, Eff_Logging L.:< es)
  => FilePath
  -> L.Eff es Int
eff_calculateFileSize path = do
  eff_logMsg $ "Calculating the size of " ++ path
  eff_tryFileSize path >>= \case
    Nothing   -> 0    <$ eff_logMsg ("Could not calculate the size of " ++ path)
    Just size -> size <$ eff_logMsg (path ++ " is " ++ show size ++ " bytes")
{-# NOINLINE eff_calculateFileSize #-}

eff_program
  :: (Eff_File L.:< es, Eff_Logging L.:< es)
  => [FilePath]
  -> L.Eff es Int
eff_program files = do
  sizes <- traverse eff_calculateFileSize files
  pure $ sum sizes
{-# NOINLINE eff_program #-}

eff_calculateFileSizes :: [FilePath] -> IO ([Text], Int)
eff_calculateFileSizes =
  L.runIO . eff_runFile . eff_runLogging . eff_program

eff_calculateFileSizesDeep :: [FilePath] -> IO ([Text], Int)
eff_calculateFileSizesDeep = L.runIO
  . runR . runR . runR . runR . runR
  . eff_runFile . eff_runLogging
  . runR . runR . runR . runR . runR
  . eff_program
  where
    runR = L.runReader ()

#endif

----------------------------------------
-- cleff

#ifdef VERSION_cleff

data Cleff_File :: C.Effect where
  Cleff_tryFileSize :: FilePath -> Cleff_File m (Maybe Int)

cleff_tryFileSize :: Cleff_File C.:> es => FilePath -> C.Eff es (Maybe Int)
cleff_tryFileSize = C.send . Cleff_tryFileSize

cleff_runFile :: C.IOE C.:> es => C.Eff (Cleff_File : es) a -> C.Eff es a
cleff_runFile = C.interpret \case
  Cleff_tryFileSize path -> liftIO $ tryGetFileSize path

data Cleff_Logging :: C.Effect where
  Cleff_logMsg :: !Text -> Cleff_Logging m ()

cleff_logMsg :: Cleff_Logging C.:> es => String -> C.Eff es ()
cleff_logMsg = C.send . Cleff_logMsg . T.pack

cleff_runLogging
  :: C.Eff (Cleff_Logging : es) a
  -> C.Eff es (a, [Text])
cleff_runLogging = C.runState [] . C.reinterpret \case
  Cleff_logMsg msg -> C.modify (msg :)

----------

cleff_calculateFileSize
  :: (Cleff_File C.:> es, Cleff_Logging C.:> es)
  => FilePath
  -> C.Eff es Int
cleff_calculateFileSize path = do
  cleff_logMsg $ "Calculating the size of " ++ path
  cleff_tryFileSize path >>= \case
    Nothing   -> 0    <$ cleff_logMsg ("Could not calculate the size of " ++ path)
    Just size -> size <$ cleff_logMsg (path ++ " is " ++ show size ++ " bytes")
{-# NOINLINE cleff_calculateFileSize #-}

cleff_program
  :: (Cleff_File C.:> es, Cleff_Logging C.:> es)
  => [FilePath]
  -> C.Eff es Int
cleff_program files = do
  sizes <- traverse cleff_calculateFileSize files
  pure $ sum sizes
{-# NOINLINE cleff_program #-}

cleff_calculateFileSizes :: [FilePath] -> IO (Int, [Text])
cleff_calculateFileSizes =
  C.runIOE . cleff_runFile . cleff_runLogging . cleff_program

cleff_calculateFileSizesDeep :: [FilePath] -> IO (Int, [Text])
cleff_calculateFileSizesDeep = C.runIOE
  . runR . runR . runR . runR . runR
  . cleff_runFile . cleff_runLogging
  . runR . runR . runR . runR . runR
  . cleff_program
  where
    runR = C.runReader ()

#endif

----------------------------------------
-- freer-simple

#ifdef VERSION_freer_simple

data FS_File r where
  FS_tryFileSize :: FilePath -> FS_File (Maybe Int)

fs_tryFileSize :: FS.Member FS_File es => FilePath -> FS.Eff es (Maybe Int)
fs_tryFileSize = FS.send . FS_tryFileSize

fs_runFile :: FS.LastMember IO es => FS.Eff (FS_File : es) a -> FS.Eff es a
fs_runFile = FS.interpret \case
  FS_tryFileSize path -> liftIO $ tryGetFileSize path

data FS_Logging r where
  FS_logMsg :: !Text -> FS_Logging ()

fs_logMsg :: FS.Member FS_Logging es => String -> FS.Eff es ()
fs_logMsg = FS.send . FS_logMsg . T.pack

fs_runLogging
  :: FS.Eff (FS_Logging : es) a
  -> FS.Eff es (a, [Text])
fs_runLogging = FS.runState [] . FS.reinterpret \case
  FS_logMsg msg -> FS.modify (msg :)

----------

fs_calculateFileSize
  :: (FS.Member FS_File es, FS.Member FS_Logging es)
  => FilePath
  -> FS.Eff es Int
fs_calculateFileSize path = do
  fs_logMsg $ "Calculating the size of " ++ path
  fs_tryFileSize path >>= \case
    Nothing   -> 0    <$ fs_logMsg ("Could not calculate the size of " ++ path)
    Just size -> size <$ fs_logMsg (path ++ " is " ++ show size ++ " bytes")
{-# NOINLINE fs_calculateFileSize #-}

fs_program
  :: (FS.Member FS_File es, FS.Member FS_Logging es)
  => [FilePath]
  -> FS.Eff es Int
fs_program files = do
  sizes <- traverse fs_calculateFileSize files
  pure $ sum sizes
{-# NOINLINE fs_program #-}

fs_calculateFileSizes :: [FilePath] -> IO (Int, [Text])
fs_calculateFileSizes =
  FS.runM . fs_runFile . fs_runLogging . fs_program

fs_calculateFileSizesDeep :: [FilePath] -> IO (Int, [Text])
fs_calculateFileSizesDeep = FS.runM
  . runR . runR . runR . runR . runR
  . fs_runFile . fs_runLogging
  . runR . runR . runR . runR . runR
  . fs_program
  where
    runR = FS.runReader ()

#endif

----------------------------------------
-- fused-effects

#ifdef VERSION_fused_effects

data FE_File :: E.Effect where
  FE_tryFileSize :: FilePath -> FE_File m (Maybe Int)

fe_tryFileSize :: FE.Has FE_File sig m => FilePath -> m (Maybe Int)
fe_tryFileSize = FE.send . FE_tryFileSize

newtype FE_FileC m a = FE_FileC { fe_runFileC :: m a }
  deriving newtype (Applicative, Functor, Monad, MonadIO)

instance
  ( MonadIO m
  , FE.Algebra sig m
  ) => FE.Algebra (FE_File FE.:+: sig) (FE_FileC m) where
  alg hdl sig ctx = case sig of
    FE.L (FE_tryFileSize path) -> (<$ ctx) <$> liftIO (tryGetFileSize path)
    FE.R other                 -> FE_FileC $ FE.alg (fe_runFileC . hdl) other ctx

data FE_Logging :: E.Effect where
  FE_logMsg :: !Text -> FE_Logging m ()

fe_logMsg :: FE.Has FE_Logging sig m => String -> m ()
fe_logMsg = FE.send . FE_logMsg . T.pack

newtype FE_LoggingC m a = FE_LoggingC { fe_runLoggingC :: FE.StateC [Text] m a }
  deriving newtype (Applicative, Functor, Monad)

instance
  ( FE.Algebra sig m
  ) => FE.Algebra (FE_Logging FE.:+: sig) (FE_LoggingC m) where
  alg hdl sig ctx = case sig of
    FE.L (FE_logMsg msg) -> FE_LoggingC $ ctx <$ FE.modify (msg :)
    FE.R other           -> FE_LoggingC $ FE.alg (fe_runLoggingC . hdl) (FE.R other) ctx

fe_runLogging :: FE_LoggingC m a -> m ([Text], a)
fe_runLogging = FE.runState [] . fe_runLoggingC

----------

fe_calculateFileSize
  :: (FE.Member FE_File sig, FE.Member FE_Logging sig, FE.Algebra sig m)
  => FilePath
  -> m Int
fe_calculateFileSize path = do
  fe_logMsg $ "Calculating the size of " ++ path
  fe_tryFileSize path >>= \case
    Nothing   -> 0    <$ fe_logMsg ("Could not calculate the size of " ++ path)
    Just size -> size <$ fe_logMsg (path ++ " is " ++ show size ++ " bytes")
{-# NOINLINE fe_calculateFileSize #-}

fe_program
  :: (FE.Member FE_File sig, FE.Member FE_Logging sig, FE.Algebra sig m)
  => [FilePath]
  -> m Int
fe_program files = do
  sizes <- traverse fe_calculateFileSize files
  pure $ sum sizes
{-# NOINLINE fe_program #-}

fe_calculateFileSizes :: [FilePath] -> IO ([Text], Int)
fe_calculateFileSizes = fe_runFileC . fe_runLogging . fe_program

fe_calculateFileSizesDeep :: [FilePath] -> IO ([Text], Int)
fe_calculateFileSizesDeep
  = runR . runR . runR . runR . runR
  . fe_runFileC . fe_runLogging
  . runR . runR . runR . runR . runR
  . fe_program
  where
    runR = FE.runReader ()

#endif

----------------------------------------
-- mtl

#ifdef VERSION_mtl

class Monad m => MonadFile m where
  mtl_tryFileSize :: FilePath -> m (Maybe Int)

newtype FileT m a = FileT { runFileT :: m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO)

instance M.MonadTrans FileT where
  lift = FileT

instance {-# OVERLAPPABLE #-}
  ( MonadFile m
  , Monad (t m)
  , M.MonadTrans t
  ) => MonadFile (t m) where
  mtl_tryFileSize = M.lift . mtl_tryFileSize

instance MonadIO m => MonadFile (FileT m) where
  mtl_tryFileSize path = liftIO $ tryGetFileSize path

class Monad m => MonadLog m where
  mtl_logMsg :: String -> m ()

newtype LoggingT m a = LoggingT (M.StateT [Text] m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, M.MonadTrans)

instance {-# OVERLAPPABLE #-}
  ( MonadLog m
  , Monad (t m)
  , M.MonadTrans t
  ) => MonadLog (t m) where
  mtl_logMsg = M.lift . mtl_logMsg

instance MonadIO m => MonadLog (LoggingT m) where
  mtl_logMsg msgS = do
    let msg = T.pack msgS
    msg `seq` LoggingT (M.modify (msg :))

runLoggingT :: LoggingT m a -> m (a, [Text])
runLoggingT (LoggingT m) = M.runStateT m []

----------

mtl_calculateFileSize :: (MonadLog m, MonadFile m) => FilePath -> m Int
mtl_calculateFileSize path = do
  mtl_logMsg $ "Calculating the size of " ++ path
  mtl_tryFileSize path >>= \case
    Nothing   -> 0    <$ mtl_logMsg ("Could not calculate the size of " ++ path)
    Just size -> size <$ mtl_logMsg (path ++ " is " ++ show size ++ " bytes")
{-# NOINLINE mtl_calculateFileSize #-}

mtl_program :: (MonadLog m, MonadFile m) => [FilePath] -> m Int
mtl_program files = do
  sizes <- traverse mtl_calculateFileSize files
  pure $ sum sizes
{-# NOINLINE mtl_program #-}

mtl_calculateFileSizes :: [FilePath] -> IO (Int, [Text])
mtl_calculateFileSizes = runFileT . runLoggingT . mtl_program

mtl_calculateFileSizesDeep :: [FilePath] -> IO (Int, [Text])
mtl_calculateFileSizesDeep
  = runR . runR . runR . runR . runR
  . runFileT . runLoggingT
  . runR . runR . runR . runR . runR
  . mtl_program
  where
    runR = flip M.runReaderT ()

#endif

----------------------------------------
-- polysemy

#ifdef VERSION_polysemy

data Poly_File :: E.Effect where
  Poly_tryFileSize :: FilePath -> Poly_File m (Maybe Int)

poly_tryFileSize :: P.Member Poly_File es => FilePath -> P.Sem es (Maybe Int)
poly_tryFileSize = P.send . Poly_tryFileSize

poly_runFile :: P.Member (P.Embed IO) es => P.Sem (Poly_File : es) a -> P.Sem es a
poly_runFile = P.interpret \case
  Poly_tryFileSize path -> P.embed $ tryGetFileSize path

data Poly_Logging :: E.Effect where
  Poly_logMsg :: !Text -> Poly_Logging m ()

poly_logMsg :: P.Member Poly_Logging es => String -> P.Sem es ()
poly_logMsg = P.send . Poly_logMsg . T.pack

poly_runLogging :: P.Sem (Poly_Logging : es) a -> P.Sem es ([Text], a)
poly_runLogging = P.runState [] . P.reinterpret \case
  Poly_logMsg msg -> P.modify (msg :)

----------

poly_calculateFileSize
  :: (P.Member Poly_File es, P.Member Poly_Logging es)
  => FilePath
  -> P.Sem es Int
poly_calculateFileSize path = do
  poly_logMsg $ "Calculating the size of " ++ path
  poly_tryFileSize path >>= \case
    Nothing   -> 0    <$ poly_logMsg ("Could not calculate the size of " ++ path)
    Just size -> size <$ poly_logMsg (path ++ " is " ++ show size ++ " bytes")
{-# NOINLINE poly_calculateFileSize #-}

poly_program
  :: (P.Member Poly_File es, P.Member Poly_Logging es)
  => [FilePath]
  -> P.Sem es Int
poly_program files = do
  sizes <- traverse poly_calculateFileSize files
  pure $ sum sizes
{-# NOINLINE poly_program #-}

poly_calculateFileSizes :: [FilePath] -> IO ([Text], Int)
poly_calculateFileSizes =
  P.runM . poly_runFile . poly_runLogging . poly_program

poly_calculateFileSizesDeep :: [FilePath] -> IO ([Text], Int)
poly_calculateFileSizesDeep = P.runM
  . runR . runR . runR . runR . runR
  . poly_runFile . poly_runLogging
  . runR . runR . runR . runR . runR
  . poly_program
  where
    runR = P.runReader ()
#endif
