{-# LANGUAGE UndecidableInstances #-}

-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2024 Sayo Koyoneda
License     :  MPL-2.0 (see the LICENSE file)
Maintainer  :  ymdfield@outlook.jp
-}
module Control.Effect.Interpreter.Heftia.Provider where

import Control.Monad.Hefty (
    Eff,
    HFunctor,
    HFunctors,
    IsHFunctor,
    MemberHBy,
    interpretH,
    raise,
    raiseNH,
    tag,
    tagH,
    untag,
    untagH,
    type (~>),
 )
import Data.Effect.Key (KeyH (KeyH))
import Data.Effect.Provider (Provider, Provider' (Provide), ProviderKey)
import Data.Effect.Provider qualified as P
import Data.Effect.Tag (type (#), type (##))
import Data.Functor.Identity (Identity (Identity))

type ProviderFix ctx i eh rh ef rf = Provider ctx i (ProviderBase ctx i eh rh ef rf)
type ProviderFix_ i eh rh ef rf = Provider Identity i (ProviderBase Identity i eh rh ef rf)

newtype ProviderBase ctx i eh rh ef rf a
    = ProviderBase
    { unProviderBase
        :: Eff (eh ': ProviderFix ctx i eh rh ef rf ': rh) (ef ': rf) a
    }
    deriving newtype (Functor, Applicative, Monad)

{-
runProvider
    :: (HFunctors eh)
    => (forall x. i -> Eff eh (e ': ef) x -> Eff eh ef (ctx x))
    -> Eff (Provider ctx i (Eff eh (e ': ef)) ': eh) ef ~> Eff eh ef
runProvider run =
    interpretH \(KeyH (Provide i f)) -> run i $ f raise

runProviderH
    :: (HFunctors eh)
    => (forall x. i -> Eff (e ': eh) ef x -> Eff eh ef (ctx x))
    -> Eff (Provider ctx i (Eff (e ': eh) ef) ': eh) ef ~> Eff eh ef
runProviderH run =
    interpretH \(KeyH (Provide i f)) -> run i $ f raiseH

runProviderHF
    :: (HFunctors rh)
    => (forall x. i -> Eff (eh ': rh) (ef ': rf) x -> Eff rh rf (ctx x))
    -> Eff (Provider ctx i (Eff (eh ': rh) (ef ': rf)) ': rh) rf ~> Eff rh rf
runProviderHF run =
    interpretH \(KeyH (Provide i f)) -> run i $ f (raiseH . raise)

runProvider_
    :: (HFunctors eh)
    => (i -> Eff eh (e ': ef) ~> Eff eh ef)
    -> Eff (Provider_ i (Eff eh (e ': ef)) ': eh) ef ~> Eff eh ef
runProvider_ run =
    interpretH \(KeyH (Provide i f)) -> fmap Identity $ run i $ f raise

runProviderH_
    :: (HFunctors eh)
    => (i -> Eff (e ': eh) ef ~> Eff eh ef)
    -> Eff (Provider_ i (Eff (e ': eh) ef) ': eh) ef ~> Eff eh ef
runProviderH_ run =
    interpretH \(KeyH (Provide i f)) -> fmap Identity $ run i $ f raiseH
-}

runProvider
    :: forall ctx i eh rh ef rf
     . (HFunctors rh)
    => ( forall x
          . i
         -> Eff (eh ': ProviderFix ctx i eh rh ef rf ': rh) (ef ': rf) x
         -> Eff (ProviderFix ctx i eh rh ef rf ': rh) rf (ctx x)
       )
    -> Eff (ProviderFix ctx i eh rh ef rf ': rh) rf ~> Eff rh rf
runProvider run = loop
  where
    loop :: Eff (ProviderFix ctx i eh rh ef rf ': rh) rf ~> Eff rh rf
    loop = interpretH \(KeyH (Provide i f)) ->
        loop . run i . unProviderBase $
            f (ProviderBase . raiseNH @2 . raise)

runProvider_
    :: forall i eh rh ef rf
     . (HFunctors rh)
    => ( i
         -> Eff (eh ': ProviderFix_ i eh rh ef rf ': rh) (ef ': rf)
            ~> Eff (ProviderFix_ i eh rh ef rf ': rh) rf
       )
    -> Eff (ProviderFix_ i eh rh ef rf ': rh) rf ~> Eff rh rf
runProvider_ run = runProvider \i m -> run i $ Identity <$> m

provide
    :: forall tag ctx i eh ef a sh bh sf bf
     . ( MemberHBy
            (ProviderKey ctx i)
            (Provider' ctx i (ProviderBase ctx i sh bh sf bf))
            eh
       , HFunctor sh
       , HFunctors bh
       , IsHFunctor sh ~ True
       )
    => i
    -> ( (Eff eh ef ~> Eff (sh ## tag ': ProviderFix ctx i sh bh sf bf ': bh) (sf # tag ': bf))
         -> Eff (sh ## tag ': ProviderFix ctx i sh bh sf bf ': bh) (sf # tag ': bf) a
       )
    -> Eff eh ef (ctx a)
provide i f =
    i P...! \runInBase ->
        ProviderBase . untag . untagH $ f $ tagH . tag . unProviderBase . runInBase

provide_
    :: forall tag i eh ef a sh bh sf bf
     . ( MemberHBy
            (ProviderKey Identity i)
            (Provider' Identity i (ProviderBase Identity i sh bh sf bf))
            eh
       , HFunctor sh
       , HFunctors bh
       , IsHFunctor sh ~ True
       )
    => i
    -> ( (Eff eh ef ~> Eff (sh ## tag ': ProviderFix_ i sh bh sf bf ': bh) (sf # tag ': bf))
         -> Eff (sh ## tag ': ProviderFix_ i sh bh sf bf ': bh) (sf # tag ': bf) a
       )
    -> Eff eh ef a
provide_ i f =
    i P..! \runInBase ->
        ProviderBase . untag . untagH $ f $ tagH . tag . unProviderBase . runInBase

{-
elabProvider
    :: (forall x. Eff (e ': eh) ef x -> Eff (e ': eh) ef (ctx x))
    -> (i -> hdls (Eff (e ': eh) ef))
    -> SubElab (Provider ctx i hdls) e eh ef
elabProvider ctx mkHandler f (Provide i withHandlerTable) =
    f $ ctx $ withHandlerTable $ mkHandler i

elabProvider_
    :: (i -> hdls (Eff (e ': eh) ef))
    -> SubElab (Provider_ i hdls) e eh ef
elabProvider_ = elabProvider $ fmap Identity

type SubElab e e' eh ef =
    forall x
     . Eff (e' ': eh) ef ~> Eff eh ef
    -> e (Eff (e' ': eh) ef) x
    -> Eff eh ef x
-}
