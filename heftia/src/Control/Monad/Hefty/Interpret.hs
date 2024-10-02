{-# LANGUAGE AllowAmbiguousTypes #-}

-- SPDX-License-Identifier: MPL-2.0

module Control.Monad.Hefty.Interpret where

import Control.Arrow ((>>>))
import Control.Effect (type (~>))
import Control.Monad.Hefty.Types (
    Eff (Op, Val),
    Elab,
    Elaborator,
    Interpreter,
    sendUnionBy,
    sendUnionHBy,
 )
import Data.Effect.HFunctor (HFunctor, hfmap)
import Data.Effect.OpenUnion.Internal (IsSuffixOf, WeakenN)
import Data.Effect.OpenUnion.Internal.FO (
    Member (prj),
    Union,
    extract,
    nil,
    weakenN,
    weakens,
    (!+),
    type (<!),
 )
import Data.Effect.OpenUnion.Internal.HO (
    MemberH (prjH),
    UnionH,
    extractH,
    hfmapUnion,
    nilH,
    weakenNH,
    weakensH,
    (!!+),
    type (<!!),
 )
import Data.FTCQueue (FTCQueue, ViewL (TOne, (:|)), tviewl, (><))

{- | Iteratively interprets all effects in an 'Eff' computation.
The function takes interpreters for higher-order effects and first-order effects, and applies them.
-}
iterAllEffHFBy
    :: forall eh ef m ans a
     . (Monad m)
    => (a -> m ans)
    -> Interpreter (UnionH eh (Eff eh ef)) m ans
    -> Interpreter (Union ef) m ans
    -> Eff eh ef a
    -> m ans
iterAllEffHFBy ret fh ff = loop
  where
    loop = \case
        Val x -> ret x
        Op u q -> either fh ff u k
          where
            k = loop . qApp q
{-# INLINE iterAllEffHFBy #-}

qApp :: FTCQueue (Eff eh ef) a b -> a -> Eff eh ef b
qApp q' x = case tviewl q' of
    TOne k -> k x
    k :| t -> case k x of
        Val y -> qApp t y
        Op u q -> Op u (q >< t)

-- | Lifts a natural transformation into an interpreter.
plain :: forall e m ans. (Monad m) => (e ~> m) -> Interpreter e m ans
plain i e k = i e >>= k
{-# INLINE plain #-}

iterEffBy :: forall e m ans a. (Monad m) => (a -> m ans) -> Interpreter e m ans -> Eff '[] '[e] a -> m ans
iterEffBy ret hdl = iterAllEffHFBy ret nilH (hdl . extract)
{-# INLINE iterEffBy #-}

iterEffHBy :: forall e m ans a. (Monad m, HFunctor e) => (a -> m ans) -> Interpreter (e (Eff '[e] '[])) m ans -> Eff '[e] '[] a -> m ans
iterEffHBy ret elb = iterAllEffHFBy ret (elb . extractH) nil
{-# INLINE iterEffHBy #-}

iterEffRecHWith :: forall e m. (Monad m, HFunctor e) => (forall ans. Elaborator e m ans) -> Eff '[e] '[] ~> m
iterEffRecHWith elb = loop
  where
    loop :: Eff '[e] '[] ~> m
    loop = iterEffHBy pure (elb . hfmap loop)
{-# INLINE iterEffRecHWith #-}

iterEffRecH :: forall e m. (Monad m, HFunctor e) => Elab e m -> Eff '[e] '[] ~> m
iterEffRecH elb = iterEffRecHWith $ plain elb
{-# INLINE iterEffRecH #-}

iterEffHFBy
    :: forall eh ef m ans a
     . (Monad m, HFunctor eh)
    => (a -> m ans)
    -> Interpreter (eh (Eff '[eh] '[ef])) m ans
    -> Interpreter ef m ans
    -> Eff '[eh] '[ef] a
    -> m ans
iterEffHFBy ret elb hdl = iterAllEffHFBy ret (elb . extractH) (hdl . extract)
{-# INLINE iterEffHFBy #-}

iterEffRecHFWith
    :: forall eh ef m
     . (Monad m, HFunctor eh)
    => (forall ans. Elaborator eh m ans)
    -> (forall ans. Interpreter ef m ans)
    -> Eff '[eh] '[ef] ~> m
iterEffRecHFWith fh ff = loop
  where
    loop :: Eff '[eh] '[ef] ~> m
    loop = iterEffHFBy pure (fh . hfmap loop) ff
{-# INLINE iterEffRecHFWith #-}

runEff :: (Monad m) => Eff '[] '[m] ~> m
runEff = iterEffBy pure $ plain id
{-# INLINE runEff #-}

runPure :: Eff '[] '[] a -> a
runPure = loop
  where
    loop = \case
        Val x -> x
        Op u _ -> case u of
            Left u' -> nilH u'
            Right u' -> nil u'
{-# INLINE runPure #-}

interpretBy
    :: forall e ef ans a
     . (a -> Eff '[] ef ans)
    -> Interpreter e (Eff '[] ef) ans
    -> Eff '[] (e ': ef) a
    -> Eff '[] ef ans
interpretBy = reinterpretBy
{-# INLINE interpretBy #-}

interpretWith
    :: forall e ef a
     . Interpreter e (Eff '[] ef) a
    -> Eff '[] (e ': ef) a
    -> Eff '[] ef a
interpretWith = reinterpretWith
{-# INLINE interpretWith #-}

interpret
    :: forall e ef
     . (e ~> Eff '[] ef)
    -> Eff '[] (e ': ef) ~> Eff '[] ef
interpret = reinterpret
{-# INLINE interpret #-}

interpretRecWith
    :: forall e ef eh a
     . (forall ans. Interpreter e (Eff eh ef) ans)
    -> Eff eh (e ': ef) a
    -> Eff eh ef a
interpretRecWith = reinterpretRecWith
{-# INLINE interpretRecWith #-}

interpretRec
    :: forall e ef eh
     . (e ~> Eff eh ef)
    -> Eff eh (e ': ef) ~> Eff eh ef
interpretRec = reinterpretRec
{-# INLINE interpretRec #-}

interpretHBy
    :: forall e eh ef ans a
     . (HFunctor e)
    => (a -> Eff eh ef ans)
    -> Interpreter (e (Eff '[e] ef)) (Eff eh ef) ans
    -> Eff '[e] ef a
    -> Eff eh ef ans
interpretHBy = reinterpretHBy
{-# INLINE interpretHBy #-}

interpretHWith
    :: forall e eh ef a
     . (HFunctor e)
    => Interpreter (e (Eff '[e] ef)) (Eff eh ef) a
    -> Eff '[e] ef a
    -> Eff eh ef a
interpretHWith = reinterpretHWith
{-# INLINE interpretHWith #-}

interpretH
    :: forall e eh ef
     . (HFunctor e)
    => (e (Eff '[e] ef) ~> Eff eh ef)
    -> Eff '[e] ef ~> Eff eh ef
interpretH = reinterpretH
{-# INLINE interpretH #-}

interpretRecHWith
    :: forall e eh ef a
     . (HFunctor e)
    => (forall ans. Elaborator e (Eff eh ef) ans)
    -> Eff (e ': eh) ef a
    -> Eff eh ef a
interpretRecHWith = reinterpretRecHWith
{-# INLINE interpretRecHWith #-}

interpretRecH
    :: forall e eh ef
     . (HFunctor e)
    => Elab e (Eff eh ef)
    -> Eff (e ': eh) ef ~> Eff eh ef
interpretRecH = reinterpretRecH
{-# INLINE interpretRecH #-}

reinterpretBy
    :: forall e ef' ef ans a
     . (ef `IsSuffixOf` ef')
    => (a -> Eff '[] ef' ans)
    -> Interpreter e (Eff '[] ef') ans
    -> Eff '[] (e ': ef) a
    -> Eff '[] ef' ans
reinterpretBy ret hdl = iterAllEffHFBy ret nilH (hdl !+ flip sendUnionBy . weakens)
{-# INLINE reinterpretBy #-}

reinterpretNBy
    :: forall n e ef' ef ans a
     . (WeakenN n ef ef')
    => (a -> Eff '[] ef' ans)
    -> Interpreter e (Eff '[] ef') ans
    -> Eff '[] (e ': ef) a
    -> Eff '[] ef' ans
reinterpretNBy ret hdl = iterAllEffHFBy ret nilH (hdl !+ flip sendUnionBy . weakenN @n)
{-# INLINE reinterpretNBy #-}

reinterpretWith
    :: forall e ef' ef a
     . (ef `IsSuffixOf` ef')
    => Interpreter e (Eff '[] ef') a
    -> Eff '[] (e ': ef) a
    -> Eff '[] ef' a
reinterpretWith = reinterpretBy pure
{-# INLINE reinterpretWith #-}

reinterpretNWith
    :: forall n e ef' ef a
     . (WeakenN n ef ef')
    => Interpreter e (Eff '[] ef') a
    -> Eff '[] (e ': ef) a
    -> Eff '[] ef' a
reinterpretNWith = reinterpretNBy @n pure
{-# INLINE reinterpretNWith #-}

reinterpret :: forall e ef' ef. (ef `IsSuffixOf` ef') => (e ~> Eff '[] ef') -> Eff '[] (e ': ef) ~> Eff '[] ef'
reinterpret f = reinterpretWith (plain f)
{-# INLINE reinterpret #-}

reinterpretN :: forall n e ef' ef. (WeakenN n ef ef') => (e ~> Eff '[] ef') -> Eff '[] (e ': ef) ~> Eff '[] ef'
reinterpretN f = reinterpretNWith @n (plain f)
{-# INLINE reinterpretN #-}

reinterpretRecWith
    :: forall e ef' ef eh a
     . (ef `IsSuffixOf` ef')
    => (forall ans. Interpreter e (Eff eh ef') ans)
    -> Eff eh (e ': ef) a
    -> Eff eh ef' a
reinterpretRecWith hdl = loop
  where
    loop :: Eff eh (e ': ef) ~> Eff eh ef'
    loop = iterAllEffHFBy pure (flip sendUnionHBy . hfmap loop) (hdl !+ flip sendUnionBy . weakens)
{-# INLINE reinterpretRecWith #-}

reinterpretRecNWith
    :: forall n e ef' ef eh a
     . (WeakenN n ef ef')
    => (forall ans. Interpreter e (Eff eh ef') ans)
    -> Eff eh (e ': ef) a
    -> Eff eh ef' a
reinterpretRecNWith hdl = loop
  where
    loop :: Eff eh (e ': ef) ~> Eff eh ef'
    loop = iterAllEffHFBy pure (flip sendUnionHBy . hfmap loop) (hdl !+ flip sendUnionBy . weakenN @n)
{-# INLINE reinterpretRecNWith #-}

reinterpretRec :: forall e ef' ef eh. (ef `IsSuffixOf` ef') => (e ~> Eff eh ef') -> Eff eh (e ': ef) ~> Eff eh ef'
reinterpretRec f = reinterpretRecWith (plain f)
{-# INLINE reinterpretRec #-}

reinterpretRecN :: forall n e ef' ef eh. (WeakenN n ef ef') => (e ~> Eff eh ef') -> Eff eh (e ': ef) ~> Eff eh ef'
reinterpretRecN f = reinterpretRecNWith @n (plain f)
{-# INLINE reinterpretRecN #-}

reinterpretHBy
    :: forall e eh ef ans a
     . (HFunctor e)
    => (a -> Eff eh ef ans)
    -> Interpreter (e (Eff '[e] ef)) (Eff eh ef) ans
    -> Eff '[e] ef a
    -> Eff eh ef ans
reinterpretHBy ret elb = iterAllEffHFBy ret (elb !!+ nilH) (flip sendUnionBy)
{-# INLINE reinterpretHBy #-}

reinterpretNHBy
    :: forall n e eh ef ans a
     . (HFunctor e, WeakenN n '[] eh)
    => (a -> Eff eh ef ans)
    -> Interpreter (e (Eff '[e] ef)) (Eff eh ef) ans
    -> Eff '[e] ef a
    -> Eff eh ef ans
reinterpretNHBy = reinterpretHBy
{-# INLINE reinterpretNHBy #-}

reinterpretHWith
    :: forall e eh ef a
     . (HFunctor e)
    => Interpreter (e (Eff '[e] ef)) (Eff eh ef) a
    -> Eff '[e] ef a
    -> Eff eh ef a
reinterpretHWith = reinterpretHBy pure
{-# INLINE reinterpretHWith #-}

reinterpretNHWith
    :: forall n e eh ef a
     . (HFunctor e, WeakenN n '[] eh)
    => Interpreter (e (Eff '[e] ef)) (Eff eh ef) a
    -> Eff '[e] ef a
    -> Eff eh ef a
reinterpretNHWith = reinterpretHWith
{-# INLINE reinterpretNHWith #-}

reinterpretH
    :: forall e eh ef
     . (HFunctor e)
    => (e (Eff '[e] ef) ~> Eff eh ef)
    -> Eff '[e] ef ~> Eff eh ef
reinterpretH elb = reinterpretHWith (plain elb)
{-# INLINE reinterpretH #-}

reinterpretNH
    :: forall n e eh ef
     . (HFunctor e, WeakenN n '[] eh)
    => (e (Eff '[e] ef) ~> Eff eh ef)
    -> Eff '[e] ef ~> Eff eh ef
reinterpretNH = reinterpretH
{-# INLINE reinterpretNH #-}

reinterpretRecHWith
    :: forall e eh eh' ef a
     . (HFunctor e, eh `IsSuffixOf` eh')
    => (forall ans. Elaborator e (Eff eh' ef) ans)
    -> Eff (e ': eh) ef a
    -> Eff eh' ef a
reinterpretRecHWith elb = loop
  where
    loop :: Eff (e ': eh) ef ~> Eff eh' ef
    loop =
        iterAllEffHFBy
            pure
            (elb . hfmap loop !!+ flip sendUnionHBy . weakensH . hfmapUnion loop)
            (flip sendUnionBy)
{-# INLINE reinterpretRecHWith #-}

reinterpretRecNHWith
    :: forall n e eh eh' ef a
     . (HFunctor e, WeakenN n eh eh')
    => (forall ans. Elaborator e (Eff eh' ef) ans)
    -> Eff (e ': eh) ef a
    -> Eff eh' ef a
reinterpretRecNHWith elb = loop
  where
    loop :: Eff (e ': eh) ef ~> Eff eh' ef
    loop =
        iterAllEffHFBy
            pure
            (elb . hfmap loop !!+ flip sendUnionHBy . weakenNH @n . hfmapUnion loop)
            (flip sendUnionBy)
{-# INLINE reinterpretRecNHWith #-}

reinterpretRecH
    :: forall e eh eh' ef
     . (HFunctor e, eh `IsSuffixOf` eh')
    => Elab e (Eff eh' ef)
    -> Eff (e ': eh) ef ~> Eff eh' ef
reinterpretRecH elb = reinterpretRecHWith (plain elb)
{-# INLINE reinterpretRecH #-}

reinterpretRecNH
    :: forall n e eh eh' ef
     . (HFunctor e, WeakenN n eh eh')
    => Elab e (Eff eh' ef)
    -> Eff (e ': eh) ef ~> Eff eh' ef
reinterpretRecNH elb = reinterpretRecNHWith @n (plain elb)
{-# INLINE reinterpretRecNH #-}

interposeBy
    :: forall e ef ans a
     . (e <! ef)
    => (a -> Eff '[] ef ans)
    -> Interpreter e (Eff '[] ef) ans
    -> Eff '[] ef a
    -> Eff '[] ef ans
interposeBy ret f = iterAllEffHFBy ret nilH \u -> maybe (`sendUnionBy` u) f (prj @e u)
{-# INLINE interposeBy #-}

interposeWith
    :: forall e ef a
     . (e <! ef)
    => Interpreter e (Eff '[] ef) a
    -> Eff '[] ef a
    -> Eff '[] ef a
interposeWith = interposeBy pure
{-# INLINE interposeWith #-}

interpose
    :: forall e ef
     . (e <! ef)
    => (e ~> Eff '[] ef)
    -> Eff '[] ef ~> Eff '[] ef
interpose f = interposeWith (plain f)
{-# INLINE interpose #-}

interposeRecWith
    :: forall e ef eh a
     . (e <! ef)
    => (forall ans. Interpreter e (Eff eh ef) ans)
    -> Eff eh ef a
    -> Eff eh ef a
interposeRecWith f = loop
  where
    loop :: Eff eh ef ~> Eff eh ef
    loop = iterAllEffHFBy
        pure
        (flip sendUnionHBy . hfmapUnion loop)
        \u -> maybe (`sendUnionBy` u) f (prj @e u)
{-# INLINE interposeRecWith #-}

interposeRec
    :: forall e ef eh
     . (e <! ef)
    => (e ~> Eff eh ef)
    -> Eff eh ef ~> Eff eh ef
interposeRec f = interposeRecWith (plain f)
{-# INLINE interposeRec #-}

interposeRecHWith
    :: forall e eh ef a
     . (e <!! eh, HFunctor e)
    => (forall ans. Elaborator e (Eff eh ef) ans)
    -> Eff eh ef a
    -> Eff eh ef a
interposeRecHWith f = loop
  where
    loop :: Eff eh ef ~> Eff eh ef
    loop =
        iterAllEffHFBy
            pure
            (hfmapUnion loop >>> \u -> maybe (`sendUnionHBy` u) f (prjH @e u))
            (flip sendUnionBy)
{-# INLINE interposeRecHWith #-}

interposeRecH
    :: forall e eh ef
     . (e <!! eh, HFunctor e)
    => Elab e (Eff eh ef)
    -> Eff eh ef ~> Eff eh ef
interposeRecH f = interposeRecHWith (plain f)
{-# INLINE interposeRecH #-}
