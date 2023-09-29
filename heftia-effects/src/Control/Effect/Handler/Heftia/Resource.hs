-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2023 Yamada Ryo
               (c) 2017 FP Complete
               (c) 2022 Fumiaki Kinoshita
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable

An elaborator for the t'Control.Effect.Class.Resource.Resource' effect class.
-}
module Control.Effect.Handler.Heftia.Resource where

import Control.Effect.Class (LiftIns (LiftIns), type (~>))
import Control.Effect.Class.Resource (ResourceS (Bracket, BracketOnExcept))
import Control.Effect.Freer (Fre, freerEffects, unFreerEffects)
import Control.Effect.Heftia (Elaborator)
import Control.Exception qualified as E
import Control.Monad.Cont (ContT (ContT), MonadPlus (mzero))
import Control.Monad.Trans.Freer (MonadTransFreer (interpretMK))
import Control.Monad.Trans.Freer.Church (FreerChurchT (FreerChurchT))
import Control.Monad.Trans.Heftia.Church (HeftiaChurchT (HeftiaChurchT))
import Data.Free.Extensible (ExtensibleUnion)
import UnliftIO (
    MonadUnliftIO,
    SomeException,
    atomically,
    newEmptyMVar,
    putMVar,
    readMVar,
    throwIO,
    withRunInIO,
 )
import UnliftIO qualified as IO
import UnliftIO.Concurrent (forkFinally, killThread)

{- | Elaborates the `Resource` effect under the `MonadUnliftIO` context.

Be aware that during the interpretation of resource acquisition, release, and the bracketed
computation, the continuation is delimited.
-}

{-  [1] Copyright (c) 2017 FP Complete

        Permission is hereby granted, free of charge, to any person obtaining
        a copy of this software and associated documentation files (the
        "Software"), to deal in the Software without restriction, including
        without limitation the rights to use, copy, modify, merge, publish,
        distribute, sublicense, and/or sell copies of the Software, and to
        permit persons to whom the Software is furnished to do so, subject to
        the following conditions:

        The above copyright notice and this permission notice shall be included
        in all copies or substantial portions of the Software.

        THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
        EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
        MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
        IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
        CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
        TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
        SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
-}
resourceToIO :: MonadUnliftIO m => Elaborator ResourceS (Fre r m)
resourceToIO = \case
    {-  The code before modification is licensed under the MIT License as
        shown in [1]. The modified code, in its entirety, is licensed under
        MPL 2.0. When redistributing, please ensure that you do not remove
        the MIT License text as indicated in [1].

        [@UnliftIO.Exception.{bracket,bracketOnError}@]
        (https://hackage.haskell.org/package/unliftio-0.2.25.0/docs/src/UnliftIO.Exception.html).
    -}

    Bracket acquire release thing ->
        frech \i -> ContT \k ->
            withRunInIO \run -> E.mask \restore -> do
                rresource <- run $ delimitIO $ interpretMK i $ unFreerEffects acquire
                continue rresource \resource -> do
                    reresult <-
                        E.try @SomeException . restore . run $
                            delimitIO (interpretMK i $ unFreerEffects $ thing resource)
                    case reresult of
                        Left e -> do
                            _ <-
                                E.try @SomeException . E.uninterruptibleMask_ . run $
                                    delimitIO (interpretMK i $ unFreerEffects $ release resource)
                            E.throwIO e
                        Right rresult -> continue rresult \result -> do
                            z <-
                                E.uninterruptibleMask_ . run $
                                    delimitIO (interpretMK i $ unFreerEffects $ release resource)
                            continue z \() -> restore . run $ k result
    BracketOnExcept acquire onError thing ->
        frech \i -> ContT \k ->
            withRunInIO \run -> E.mask \restore -> do
                rresource <- run $ delimitIO $ interpretMK i $ unFreerEffects acquire
                continue rresource \resource -> do
                    reresult <-
                        E.try @SomeException . restore . run $
                            delimitIO (interpretMK i $ unFreerEffects $ thing resource)
                    case reresult of
                        Left e -> do
                            _ <-
                                E.try @SomeException . E.uninterruptibleMask_ . run $
                                    delimitIO (interpretMK i $ unFreerEffects $ onError resource)
                            E.throwIO e
                        Right rresult ->
                            continue rresult $ restore . run . k

{- |
By forking a thread, the continuation monad is delimited.
If the continuation monad discards the continuation, `Left` is returned.
If it continues without discarding, `Right` is returned.
-}

{-  [2] Copyright Fumiaki Kinoshita (c) 2021

        All rights reserved.

        Redistribution and use in source and binary forms, with or without
        modification, are permitted provided that the following conditions are met:

            * Redistributions of source code must retain the above copyright
            notice, this list of conditions and the following disclaimer.

            * Redistributions in binary form must reproduce the above
            copyright notice, this list of conditions and the following
            disclaimer in the documentation and/or other materials provided
            with the distribution.

            * Neither the name of Fumiaki Kinoshita nor the names of other
            contributors may be used to endorse or promote products derived
            from this software without specific prior written permission.

        THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
        "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
        LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
        A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
        OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
        SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
        LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
        DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
        THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
        (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
        OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}
delimitIO :: MonadUnliftIO m => ContT r m a -> m (Either r a)
delimitIO (ContT f) = do
    {-  The code before modification is licensed under the BSD3 License as
        shown in [2]. The modified code, in its entirety, is licensed under
        MPL 2.0. When redistributing, please ensure that you do not remove
        the BSD3 License text as indicated in [2].

        [@Oath.oath@](https://hackage.haskell.org/package/oath-0.1.1/docs/src/Oath.html#oath)
    -}

    v <- newEmptyMVar
    tid <-
        forkFinally
            do
                r <- f \a -> do
                    putMVar v $ Right $ Right a
                    atomically mzero
                pure $ Left r
            (putMVar v)
    let await = readMVar v >>= either throwIO pure
    await `IO.finally` killThread tid

{-
A version that doesn't fork threads.
It was not adopted due to the potential risk of exceptions being blocked by @f@.

delimitIO :: MonadUnliftIO m => ContT r m a -> m (Either r a)
delimitIO (ContT f) = do
    v <- newEmptyTMVarIO
    ( Left <$> f \a -> atomically do
            putTMVar v a
            throwSTM $ toDyn ()
        )
        `catch` \(e :: Dynamic) -> do
            ma <- atomically $ tryReadTMVar v
            case ma of
                Just a -> pure $ Right a
                Nothing -> throwIO e
-}

-- | Continues from `delimitIO`.
continue :: Applicative f => Either r a -> (a -> f r) -> f r
continue e k =
    case e of
        Left r -> pure r
        Right x -> k x

frech :: (forall r. (ExtensibleUnion es ~> ContT r m) -> ContT r m a) -> Fre es m a
frech f = freerEffects $ FreerChurchT $ HeftiaChurchT \i -> f $ i . LiftIns
