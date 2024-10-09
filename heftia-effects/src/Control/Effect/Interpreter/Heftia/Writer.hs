{-# LANGUAGE AllowAmbiguousTypes #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2023 Sayo Koyoneda
License     :  MPL-2.0 (see the LICENSE file)
Maintainer  :  ymdfield@outlook.jp
Portability :  portable

Interpreter and elaborator for the t'Data.Effect.Writer.Writer' effect class.
See [README.md](https://github.com/sayo-hs/heftia/blob/master/README.md).
-}
module Control.Effect.Interpreter.Heftia.Writer where

import Control.Effect (type (~>))
import Control.Monad.Hefty (
    Eff,
    StateInterpreter,
    interpose,
    interposeStateBy,
    interpretH,
    interpretStateBy,
    send,
    type (<|),
 )
import Data.Effect.Writer (Tell (Tell), WriterH (Censor, Listen), tell)

-- | 'Writer' effect handler with post-applying censor semantics.
runWriterPost :: (Monoid w) => Eff '[WriterH w] (Tell w ': ef) a -> Eff '[] ef (w, a)
runWriterPost = runTell . runWriterHPost

-- | 'Writer' effect handler with pre-applying censor semantics.
runWriterPre :: (Monoid w) => Eff '[WriterH w] (Tell w ': ef) a -> Eff '[] ef (w, a)
runWriterPre = runTell . runWriterHPre

runTell :: (Monoid w) => Eff '[] (Tell w ': ef) a -> Eff '[] ef (w, a)
runTell = interpretStateBy mempty (curry pure) handleTell

handleTell :: (Monoid w) => StateInterpreter w (Tell w) (Eff '[] ef) (w, a)
handleTell (Tell w') w k = k (w <> w') ()
{-# INLINE handleTell #-}

runWriterHPost :: (Monoid w, Tell w <| ef) => Eff '[WriterH w] ef ~> Eff '[] ef
runWriterHPost = interpretH \case
    Listen m -> listen m
    Censor f m -> censorPost f m

runWriterHPre :: (Monoid w, Tell w <| ef) => Eff '[WriterH w] ef ~> Eff '[] ef
runWriterHPre = interpretH \case
    Listen m -> listen m
    Censor f m -> censorPre f m

{- | Retrieves the monoidal value accumulated by v'tell' within the given action.
The v'tell' effect is not consumed and remains intact.
-}
listen
    :: forall w ef a
     . (Tell w <| ef, Monoid w)
    => Eff '[] ef a
    -> Eff '[] ef (w, a)
listen =
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

censorPost
    :: forall w ef
     . (Tell w <| ef, Monoid w)
    => (w -> w)
    -> Eff '[] ef ~> Eff '[] ef
censorPost f m = do
    (w, a) <- confiscate m
    tell $ f w
    pure a

censorPre
    :: forall w eh ef
     . (Tell w <| ef, Monoid w)
    => (w -> w)
    -> Eff eh ef ~> Eff eh ef
censorPre f = interpose @(Tell w) \(Tell w) -> tell $ f w
