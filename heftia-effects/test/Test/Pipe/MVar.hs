-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Test.Pipe.MVar where

import Control.Effect (type (~>))
import Control.Effect.ExtensibleFinal (type (!!))
import Control.Effect.Handler.Heftia.Concurrent.Pipe.IO (runAsyncPipe)
import Control.Effect.Handler.Heftia.Concurrent.Pipe.IO.TVar.MVar (runMVarPipeLine)
import Control.Effect.Handler.Heftia.Unlift (runUnliftIO)
import Control.Exception (BlockedIndefinitelyOnMVar (BlockedIndefinitelyOnMVar))
import Data.Effect.Concurrent.Pipe (
    Consume,
    Feed,
    PipeF,
    PipeH,
    PipeLineF,
    PipeLineH,
    consume,
    distributePipe_,
    feed,
    passthrough,
    pipeLoop,
    splitPipe_,
    unmaskPipe_,
    (*|*),
    (*|*>),
    (*|>),
    (*||),
    (|*>),
    (|>),
 )
import Data.Effect.HFunctor ((:+:))
import Data.Effect.Unlift (UnliftIO)
import Data.Free.Sum (type (+))
import Data.Function ((&))
import Data.Void (Void)
import Test.Hspec (Spec, describe, runIO, shouldBe, shouldContain, shouldThrow)
import UnliftIO (liftIO, newEmptyMVar)

spec_direct :: Spec
spec_direct = describe "direct pipe" $ runIO do
    runPipeLine @String do
        unmaskPipe_ @String do
            let s = "PIPE TEST"
            ((), s') <- feed s *|*> consume
            liftIO $ s' `shouldBe` s

spec_mask :: Spec
spec_mask = describe "masked pipe" $ runIO do
    runPipeLine @String (feed "PIPE TEST" *|*> consume @String)
        `shouldThrow` \BlockedIndefinitelyOnMVar -> True

spec_passthrough :: Spec
spec_passthrough = describe "passthrough pipe" $ runIO do
    runPipeLine @String do
        unmaskPipe_ @String do
            let s = "PIPE TEST"
            (_, s') <- feed s *|> passthrough |> passthrough |> passthrough |*> consume
            liftIO $ s' `shouldBe` s

spec_distribute :: Spec
spec_distribute = describe "distribute pipe" $ runIO do
    runPipeLine @String do
        unmaskPipe_ @String do
            let s = "PIPE TEST"
            ((), ss) <- feed s *|*> distributePipe_ @String (consume *|* consume)
            liftIO $ ss `shouldBe` (s, s)

spec_default_is_split :: Spec
spec_default_is_split = describe "default pipe branch mode is split" $ runIO do
    runPipeLine @String do
        unmaskPipe_ @String do
            let s1 = "PIPE TEST 1"
                s2 = "PIPE TEST 2"
            (_, ss) <- (feed s1 *> feed s2) *|*> (consume *|* consume)
            liftIO $ [(s1, s2), (s2, s1)] `shouldContain` [ss]

spec_split :: Spec
spec_split = describe "split pipe" $ runIO do
    runPipeLine @String do
        unmaskPipe_ @String do
            let s1 = "PIPE TEST 1"
                s2 = "PIPE TEST 2"
            (_, ss) <- (feed s1 *> feed s2) *|*> splitPipe_ @String (consume *|* consume)
            liftIO $ [(s1, s2), (s2, s1)] `shouldContain` [ss]

spec_loop :: Spec
spec_loop = describe "loop pipe" $ runIO do
    runPipeLine @String do
        unmaskPipe_ @String do
            let s = "PIPE TEST"
            s' <- pipeLoop @String (feed s *> consume)
            liftIO $ s' `shouldBe` s

{-
                               +--> {consume} ---+
                               |                 |
    +--> {feed s *> consume} --+                 +-->+
    |                          |                 |   |
    |                          +--{passthrough}--+   |
    |                                                |
    +<-----------------------------------------------+
-}
spec_complex :: Spec
spec_complex = describe "complex pipe circuit" $ runIO do
    runPipeLine @String do
        unmaskPipe_ @String do
            let s = "PIPE TEST"
            r <-
                pipeLoop @String $
                    (feed s *> consume) *|*> distributePipe_ @String (consume *|| passthrough @Void)
            liftIO $ r `shouldBe` (s, (s, Nothing))

runPipeLine ::
    forall a.
    PipeLineH a :+: PipeH :+: UnliftIO
        !! PipeLineF a + Feed a + Consume a + PipeF + IO
        ~> IO
runPipeLine a = do
    i <- newEmptyMVar
    o <- newEmptyMVar
    a
        & runMVarPipeLine i o
        & runAsyncPipe
        & runUnliftIO
