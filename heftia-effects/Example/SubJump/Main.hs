-- SPDX-License-Identifier: MPL-2.0

module Main where

import Control.Effect (type (<:), type (<<:))
import Control.Effect.Key (SendFOEBy, key)
import Control.Monad.Extra (whenM)
import Control.Monad.Hefty (liftIO, runEff, unkey, (&))
import Control.Monad.Hefty.Reader (runAsk, runLocal, runReader)
import Control.Monad.Hefty.State (askToGet, evalState, localToState)
import Control.Monad.Hefty.SubJump (SubJump', SubJumpKey, evalSubJump, getCC)
import Control.Monad.IO.Class (MonadIO)
import Data.Effect.Reader (Ask, Local, ask, local)
import Data.Effect.State (State, get'', modify)
import Data.Functor ((<&>))

prog
    :: ( SendFOEBy SubJumpKey (SubJump' ref) m
       , Ask Int <: m
       , Local Int <<: m
       , SendFOEBy "counter" (State Int) m
       , MonadIO m
       )
    => m ()
prog = do
    k <- getCC
    env <- ask @Int
    liftIO $ putStrLn $ "[local scope outer] env = " ++ show env
    local @Int (* 2) do
        whenM (get'' @"counter" <&> (< 5)) do
            modify (+ 1) & key @"counter"
            env' <- ask @Int
            liftIO $ putStrLn $ "[local scope inner] env = " ++ show env'
            k

main :: IO ()
main = do
    putStrLn "[handleAskThenSubJump]"
    handleAskThenSubJump

    putStrLn ""
    putStrLn "[handleSubJumpThenAsk]"
    handleSubJumpThenAsk

    putStrLn ""
    putStrLn "[interpretReaderAsState]"
    interpretReaderAsState

{-
[handleAskThenSubJump]
[local scope outer] env = 1
[local scope inner] env = 2
[local scope outer] env = 1
[local scope inner] env = 2
[local scope outer] env = 1
[local scope inner] env = 2
[local scope outer] env = 1
[local scope inner] env = 2
[local scope outer] env = 1
[local scope inner] env = 2
[local scope outer] env = 1

[handleSubJumpThenAsk]
[local scope outer] env = 1
[local scope inner] env = 2
[local scope outer] env = 1
[local scope inner] env = 2
[local scope outer] env = 1
[local scope inner] env = 2
[local scope outer] env = 1
[local scope inner] env = 2
[local scope outer] env = 1
[local scope inner] env = 2
[local scope outer] env = 1

[interpretReaderAsState]
[local scope outer] env = 1
[local scope inner] env = 2
[local scope outer] env = 2
[local scope inner] env = 4
[local scope outer] env = 4
[local scope inner] env = 8
[local scope outer] env = 8
[local scope inner] env = 16
[local scope outer] env = 16
[local scope inner] env = 32
[local scope outer] env = 32
-}
handleAskThenSubJump :: IO ()
handleAskThenSubJump =
    prog
        & runReader @Int 1
        & evalSubJump
        & (evalState 0 . unkey @"counter")
        & runEff

{- |
Unlike the example of ShiftReset, interpreting SubJump first or interpreting it later results in the same behavior.
This is because the `getCC` version of SubJump differs from that of ShiftReset in that it returns a "pseudo current continuation."
While `ShiftReset.getCC` returns the real current continuation,
`SubJump.getCC` returns an action that contains only the operation `SubJump.jump` which jumps to the control flow at that point in time.
That is, `SubJump.getCC` can be represented in the following pseudo-code:

@
SubJump.getCC = pure $ SubJump.jump (goto label reference to the code line at the point where this SubJump.getCC is called)
@

First, `runLocal` modifies all `ask` within the local scope.
In the case of ShiftReset, since `getCC` returns the real current continuation, the modification is also applied to `ask` within the continuation.
However, in this case, the `k` returned by `getCC` only contains `SubJump.jump`, so it is not subject to modification.
-}
handleSubJumpThenAsk :: IO ()
handleSubJumpThenAsk =
    prog
        & runLocal @Int
        & evalSubJump
        & runAsk @Int 1
        & (evalState 0 . unkey @"counter")
        & runEff

{- |
...In contrast to the above behavior, if you want to change it, you can achieve this by converting the `Local`/`Ask` effects into `State` effects and changing the semantics.
In this case, the semantics of `local` are transformed not to modify `ask` within the scope, but instead to save the current environment state value,
overwrite the state, and restore the saved value after the scope ends (`localToState`).
-}
interpretReaderAsState :: IO ()
interpretReaderAsState =
    prog
        & localToState @Int
        & askToGet @Int
        & evalSubJump
        & evalState @Int 1
        & (evalState 0 . unkey @"counter")
        & runEff
