-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2023 Sayo contributors
License     :  MPL-2.0 (see the LICENSE file)
Maintainer  :  ymdfield@outlook.jp

Interpreters for the [Writer]("Data.Effect.Writer") effects.
-}
module Control.Monad.Hefty.Writer (
    module Control.Monad.Hefty.Writer,
    module Data.Effect.Writer,
)
where

import Control.Monad.Hefty (
    Eff,
    FOEs,
    In,
    StateHandler,
    interposeStateInBy,
    interpret,
    interpretStateBy,
    send,
 )
import Data.Effect.Writer

-- | Interpret the [Writer]("Data.Effect.Writer") effects with post-applying censor semantics.
runWriterPost :: (Monoid w, FOEs es) => Eff (WriterH w ': Tell w ': es) a -> Eff es (w, a)
runWriterPost = runTell . runWriterHPost

-- | Interpret the [Writer]("Data.Effect.Writer") effects with pre-applying censor semantics.
runWriterPre :: (Monoid w, FOEs es) => Eff (WriterH w ': Tell w ': es) a -> Eff es (w, a)
runWriterPre = runTell . runWriterHPre

-- | Interpret the t'Tell' effect.
runTell :: (Monoid w, FOEs es) => Eff (Tell w ': es) a -> Eff es (w, a)
runTell = interpretStateBy mempty (curry pure) handleTell

-- | A handler function for the t'Tell' effect.
handleTell :: (Monoid w) => StateHandler w (Tell w) f g (w, a)
handleTell (Tell w') w k = k (w <> w') ()
{-# INLINE handleTell #-}

-- | Interpret the 'WriterH' effect with post-applying censor semantics.
runWriterHPost :: (Monoid w, Tell w `In` es, FOEs es) => Eff (WriterH w ': es) a -> Eff es a
runWriterHPost = interpret \case
    Listen m -> intercept m
    Censor f m -> censorPost f m

-- | Interpret the 'WriterH' effect with pre-applying censor semantics.
runWriterHPre :: (Monoid w, Tell w `In` es, FOEs es) => Eff (WriterH w ': es) a -> Eff es a
runWriterHPre = interpret \case
    Listen m -> intercept m
    Censor f m -> censorPre f m

{- | Retrieves the monoidal value accumulated by v'tell' within the given action.
The v'tell' effect is not consumed and remains intact.
-}
intercept
    :: forall w es a
     . (Tell w `In` es, Monoid w, FOEs es)
    => Eff es a
    -> Eff es (w, a)
intercept =
    interposeStateInBy @_ @(Tell w)
        mempty
        (curry pure)
        \e@(Tell _) w k -> do
            () <- send e
            handleTell e w k

{- | Consumes all the v'tell' effects from the specified @Tell w@ slot within the
given action and returns the accumulated monoidal value along with the result.
-}
confiscate
    :: forall w es a
     . (Tell w `In` es, Monoid w, FOEs es)
    => Eff es a
    -> Eff es (w, a)
confiscate = interposeStateInBy mempty (curry pure) handleTell

-- | 'censor' with post-applying semantics.
censorPost
    :: forall w a es
     . (Tell w `In` es, Monoid w, FOEs es)
    => (w -> w)
    -> Eff es a
    -> Eff es a
censorPost f m = do
    (w, a) <- confiscate m
    tell'_ $ f w
    pure a
