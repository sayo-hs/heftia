{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}

-- SPDX-License-Identifier: MPL-2.0 AND BSD-3-Clause

{- |
Copyright   :  (c) 2024 Sayo contributors
               (c) The University of Glasgow 2004-2008
License     :  MPL-2.0 (see the LICENSE file) AND BSD-3-Clause
Maintainer  :  ymdfield@outlook.jp

Effects for well-typed subprocess.
-}
module Control.Monad.Hefty.Concurrent.Subprocess (
    module Control.Monad.Hefty.Concurrent.Subprocess,
    module Control.Monad.Hefty.Provider,
    module System.Process,
    module System.Process.Internals,
    module System.IO,
    module System.Exit,
    module Data.ByteString,
)
where

import Control.Applicative ((<|>))
import Control.Concurrent (forkIO)
import Control.Monad (liftM2)
import Control.Monad.Hefty (Eff, Effect, Emb, Freer, PolyHFunctors, interpret, liftIO, makeEffectF, (&), (:>))
import Control.Monad.Hefty.Provider
import Control.Monad.Hefty.Unlift (UnliftIO, withRunInIO)
import Data.ByteString (ByteString, hGet, hGetNonBlocking, hPut)
import Data.ByteString qualified as BS
import Data.Function (fix)
import Data.Maybe (fromJust, isNothing)
import System.Exit (ExitCode)
import System.IO (Handle)
import System.Process (CmdSpec (RawCommand, ShellCommand))
import System.Process qualified as Raw
import System.Process.Internals (GroupID, UserID)
import UnliftIO (TMVar, atomically, finally, mask, newEmptyTMVarIO, putTMVar, readTMVar, tryReadTMVar, uninterruptibleMask_)
import UnliftIO.Concurrent (ThreadId, killThread)
import UnliftIO.Process (terminateProcess, waitForProcess)

data Subprocess p :: Effect where
    WriteStdin :: ByteString -> Subprocess ('SubprocMode 'Piped o e lp 'Kill) f ()
    TryWriteStdin :: ByteString -> Subprocess ('SubprocMode 'Piped o e lp ls) f Bool
    ReadStdout :: Subprocess ('SubprocMode i 'Piped e lp ls) f ByteString
    ReadStderr :: Subprocess ('SubprocMode i o 'Piped lp ls) f ByteString
    PollSubproc :: Subprocess ('SubprocMode i o e lp 'Wait) f (Maybe ExitCode)

data SubprocMode = SubprocMode StreamMode StreamMode StreamMode Lifecycle Lifecycle

data StreamMode = Piped | NoPipe
data Lifecycle = Kill | Wait

makeEffectF ''Subprocess

type SubprocProvider es = Scoped Freer SubprocResult CreateProcess '[Subprocess] es

data SubprocResult p a where
    RaceResult :: Either ExitCode a -> SubprocResult ('SubprocMode i o e 'Kill 'Kill) a
    SubprocResult :: ExitCode -> Maybe a -> SubprocResult ('SubprocMode i o e 'Wait 'Kill) a
    ScopeResult :: Maybe ExitCode -> a -> SubprocResult ('SubprocMode i o e 'Kill 'Wait) a
    SubprocScopeResult :: ExitCode -> a -> SubprocResult ('SubprocMode i o e 'Wait 'Wait) a

deriving stock instance (Show a) => Show (SubprocResult p a)
deriving stock instance (Eq a) => Eq (SubprocResult p a)

runSubprocIO :: (UnliftIO :> es, Emb IO :> es, PolyHFunctors es) => Eff (SubprocProvider es ': es) a -> Eff es a
runSubprocIO =
    runRegionScoped \cp@CreateProcess {subprocLifecycle, scopeLifecycle} m ->
        withRunInIO \run -> do
            (hi, ho, he, ph) <- Raw.createProcess (toRawCreateProcess cp) & liftIO
            procStatus <- newEmptyTMVarIO
            scopeStatus <- newEmptyTMVarIO
            mask \restore -> do
                let
                    runThread :: TMVar a -> IO a -> IO ThreadId
                    runThread var a = forkIO $ atomically . putTMVar var =<< a

                tScope <- runThread scopeStatus $ restore . run $ do
                    m
                        & interpret \case
                            WriteStdin s -> hPut (fromJust hi) s & liftIO
                            TryWriteStdin s -> do
                                stat <- atomically $ tryReadTMVar procStatus
                                if isNothing stat
                                    then do
                                        hPut (fromJust hi) s & liftIO
                                        pure True
                                    else pure False
                            ReadStdout -> hRead (fromJust ho) & liftIO
                            ReadStderr -> hRead (fromJust he) & liftIO
                            PollSubproc -> atomically $ tryReadTMVar procStatus

                _ <- runThread procStatus $ waitForProcess ph

                finally
                    case (subprocLifecycle, scopeLifecycle) of
                        (WaitMode, WaitMode) ->
                            liftM2
                                SubprocScopeResult
                                (atomically $ readTMVar procStatus)
                                (atomically $ readTMVar scopeStatus)
                        (WaitMode, KillMode) -> do
                            exitCode <- atomically $ readTMVar procStatus
                            scopeResult <- atomically $ tryReadTMVar scopeStatus
                            pure $ SubprocResult exitCode scopeResult
                        (KillMode, WaitMode) -> do
                            scopeResult <- atomically $ readTMVar scopeStatus
                            exitCode <- atomically $ tryReadTMVar procStatus
                            pure $ ScopeResult exitCode scopeResult
                        (KillMode, KillMode) ->
                            RaceResult
                                <$> atomically
                                    ( (Left <$> readTMVar procStatus)
                                        <|> (Right <$> readTMVar scopeStatus)
                                    )
                    do
                        uninterruptibleMask_ do
                            terminateProcess ph
                            killThread tScope
                        atomically $ readTMVar procStatus

hRead :: Handle -> IO ByteString
hRead h = flip fix BS.empty \next acc -> do
    s <- hGet h chunkSize
    if BS.null s
        then pure acc
        else next $ acc <> s

chunkSize :: Int
chunkSize = 4096

data CreateProcess p where
    CreateProcess
        :: { cmdspec :: CmdSpec
            -- ^ Executable & arguments, or shell command.  If 'cwd' is 'Nothing', relative paths are resolved with respect to the current working directory.  If 'cwd' is provided, it is implementation-dependent whether relative paths are resolved with respect to 'cwd' or the current working directory, so absolute paths should be used to ensure portability.
           , stdin :: StdStream i
            -- ^ How to determine stdin
           , stdout :: StdStream o
            -- ^ How to determine stdout
           , stderr :: StdStream e
            -- ^ How to determine stderr
           , subprocLifecycle :: LifecycleMode lp
            -- ^ Whether to kill the subprocess or wait when the scope's computation finishes first.
           , scopeLifecycle :: LifecycleMode ls
            -- ^ Whether to cancel the scope's computation or wait when the subprocess finishes first.
           , cwd :: Maybe FilePath
            -- ^ Optional path to the working directory for the new process
           , env :: Maybe [(String, String)]
            -- ^ Optional environment (otherwise inherit from the current process)
           , closeFds :: Bool
            -- ^ Close all file descriptors except stdin, stdout and stderr in the new process (on Windows, only works if std_in, std_out, and std_err are all Inherit). This implementation will call close on every fd from 3 to the maximum of open files, which can be slow for high maximum of open files. XXX verify what happens with fds in nodejs child processes
           , createGroup :: Bool
            -- ^ Create a new process group. On JavaScript this also creates a new session.
           , delegateCtlc :: Bool
            -- ^ Delegate control-C handling. Use this for interactive console processes to let them handle control-C themselves (see below for details).
           , detachConsole :: Bool
            -- ^ Use the windows DETACHED_PROCESS flag when creating the process; does nothing on other platforms.
           , createNewConsole :: Bool
            -- ^ Use the windows CREATE_NEW_CONSOLE flag when creating the process; does nothing on other platforms.
           , newSession :: Bool
            -- ^ Use posix setsid to start the new process in a new session; starts process in a new session on JavaScript; does nothing on other platforms.
           , childGroup :: Maybe GroupID
            -- ^ Use posix setgid to set child process's group id; works for JavaScript when system running nodejs is posix. does nothing on other platforms.
           , childUser :: Maybe UserID
            -- ^ Use posix setuid to set child process's user id; works for JavaScript when system running nodejs is posix. does nothing on other platforms.
           , useProcessJobs :: Bool
            -- ^ On Windows systems this flag indicates that we should wait for the entire process tree
            --   to finish before unblocking. On POSIX systems this flag is ignored. See $exec-on-windows for details.
           }
        -> CreateProcess ('SubprocMode i o e lp ls)

data StdStream s where
    CreatePipe :: StdStream 'Piped
        -- ^ Create a new pipe.  The returned
        -- @Handle@ will use the default encoding
        -- and newline translation mode (just
        -- like @Handle@s created by @openFile@).
    Inherit :: StdStream 'NoPipe
        -- ^ Inherit Handle from parent
    UseHandle
        :: Handle
        -> StdStream 'NoPipe
        -- ^ Use the supplied Handle
    NoStream :: StdStream 'NoPipe
        -- ^ Close the stream's file descriptor without
        -- passing a Handle. On POSIX systems this may
        -- lead to strange behavior in the child process
        -- because attempting to read or write after the
        -- file has been closed throws an error. This
        -- should only be used with child processes that
        -- don't use the file descriptor at all. If you
        -- wish to ignore the child process's output you
        -- should either create a pipe and drain it
        -- manually or pass a @Handle@ that writes to
        -- @\/dev\/null@.

data LifecycleMode t where
    KillMode :: LifecycleMode 'Kill
    WaitMode :: LifecycleMode 'Wait

process :: FilePath -> [String] -> CreateProcess ('SubprocMode 'NoPipe 'NoPipe 'NoPipe 'Wait 'Wait)
process cmd args = command $ RawCommand cmd args

shell :: String -> CreateProcess ('SubprocMode 'NoPipe 'NoPipe 'NoPipe 'Wait 'Wait)
shell = command . ShellCommand

command :: CmdSpec -> CreateProcess ('SubprocMode 'NoPipe 'NoPipe 'NoPipe 'Wait 'Wait)
command cmdspec = commandWith cmdspec Inherit Inherit Inherit WaitMode WaitMode

commandWith
    :: CmdSpec
    -> StdStream i
    -> StdStream o
    -> StdStream e
    -> LifecycleMode lp
    -> LifecycleMode ls
    -> CreateProcess ('SubprocMode i o e lp ls)
commandWith cmdspec stdin stdout stderr subprocLifecycle scopeLifecycle =
    CreateProcess
        { cmdspec = cmdspec
        , stdin = stdin
        , stdout = stdout
        , stderr = stderr
        , subprocLifecycle
        , scopeLifecycle
        , cwd = Nothing
        , env = Nothing
        , closeFds = False
        , createGroup = False
        , delegateCtlc = False
        , detachConsole = False
        , createNewConsole = False
        , newSession = False
        , childGroup = Nothing
        , childUser = Nothing
        , useProcessJobs = False
        }

toRawCreateProcess :: CreateProcess stdio -> Raw.CreateProcess
toRawCreateProcess (CreateProcess {..}) =
    Raw.CreateProcess
        { cmdspec = cmdspec
        , cwd = cwd
        , env = env
        , std_in = toRawStdStream stdin
        , std_out = toRawStdStream stdout
        , std_err = toRawStdStream stderr
        , close_fds = closeFds
        , create_group = createGroup
        , delegate_ctlc = delegateCtlc
        , detach_console = detachConsole
        , create_new_console = createNewConsole
        , new_session = newSession
        , child_group = childGroup
        , child_user = childUser
        , use_process_jobs = useProcessJobs
        }

toRawStdStream :: StdStream pipe -> Raw.StdStream
toRawStdStream = \case
    CreatePipe -> Raw.CreatePipe
    Inherit -> Raw.Inherit
    UseHandle h -> Raw.UseHandle h
    NoStream -> Raw.NoStream
