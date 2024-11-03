-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2023 Sayo Koyoneda
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
    StateInterpreter,
    interpose,
    interposeStateBy,
    interpretH,
    interpretStateBy,
    send,
    type (<|),
    type (~>),
 )
import Data.Effect.Writer

-- | Interpret the [Writer]("Data.Effect.Writer") effects with post-applying censor semantics.
runWriterPost :: (Monoid w) => Eff '[WriterH w] (Tell w ': ef) a -> Eff '[] ef (w, a)
runWriterPost = runTell . runWriterHPost

-- | Interpret the [Writer]("Data.Effect.Writer") effects with pre-applying censor semantics.
runWriterPre :: (Monoid w) => Eff '[WriterH w] (Tell w ': ef) a -> Eff '[] ef (w, a)
runWriterPre = runTell . runWriterHPre

-- | Interpret the t'Tell' effect.
runTell :: (Monoid w) => Eff '[] (Tell w ': ef) a -> Eff '[] ef (w, a)
runTell = interpretStateBy mempty (curry pure) handleTell

-- | A handler function for the 'Tell' effect.
handleTell :: (Monoid w) => StateInterpreter w (Tell w) (Eff '[] ef) (w, a)
handleTell (Tell w') w k = k (w <> w') ()
{-# INLINE handleTell #-}

-- | Interpret the 'WriterH' effect with post-applying censor semantics.
runWriterHPost :: (Monoid w, Tell w <| ef) => Eff '[WriterH w] ef ~> Eff '[] ef
runWriterHPost = interpretH \case
    Listen m -> intercept m
    Censor f m -> censorPost f m

-- | Interpret the 'WriterH' effect with pre-applying censor semantics.
runWriterHPre :: (Monoid w, Tell w <| ef) => Eff '[WriterH w] ef ~> Eff '[] ef
runWriterHPre = interpretH \case
    Listen m -> intercept m
    Censor f m -> censorPre f m

{- | Retrieves the monoidal value accumulated by v'tell' within the given action.
The v'tell' effect is not consumed and remains intact.
-}
intercept
    :: forall w ef a
     . (Tell w <| ef, Monoid w)
    => Eff '[] ef a
    -> Eff '[] ef (w, a)
intercept =
    interposeStateBy @_ @(Tell w)
        mempty
        (curry pure)
        \e@(Tell _) w k -> do
            () <- send e
            handleTell e w k

{- | Consumes all the v'tell' effects from the specified @Tell w@ slot within the
given action and returns the accumulated monoidal value along with the result.
-}
confiscate
    :: forall w ef a
     . (Tell w <| ef, Monoid w)
    => Eff '[] ef a
    -> Eff '[] ef (w, a)
confiscate = interposeStateBy mempty (curry pure) handleTell

-- | 'censor' with post-applying semantics.
censorPost
    :: forall w ef
     . (Tell w <| ef, Monoid w)
    => (w -> w)
    -> Eff '[] ef ~> Eff '[] ef
censorPost f m = do
    (w, a) <- confiscate m
    tell $ f w
    pure a

-- | 'censor' with pre-applying semantics.
censorPre
    :: forall w eh ef
     . (Tell w <| ef, Monoid w)
    => (w -> w)
    -> Eff eh ef ~> Eff eh ef
censorPre f = interpose @(Tell w) \(Tell w) -> tell $ f w
