{-# LANGUAGE UndecidableInstances #-}

-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2024 Sayo Koyoneda
License     :  MPL-2.0 (see the LICENSE file)
Maintainer  :  ymdfield@outlook.jp
-}
module Control.Monad.Hefty.Provider (
    module Control.Monad.Hefty.Provider,
    module Data.Effect.Provider,
)
where

import Control.Monad.Hefty (
    Eff,
    HFunctor,
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
import Data.Effect.Provider
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

runProvider
    :: forall ctx i eh rh ef rf
     . ( forall x
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
     . ( i
         -> Eff (eh ': ProviderFix_ i eh rh ef rf ': rh) (ef ': rf)
            ~> Eff (ProviderFix_ i eh rh ef rf ': rh) rf
       )
    -> Eff (ProviderFix_ i eh rh ef rf ': rh) rf ~> Eff rh rf
runProvider_ run = runProvider \i m -> run i $ Identity <$> m

scope
    :: forall tag ctx i eh ef a sh bh sf bf
     . ( MemberHBy
            (ProviderKey ctx i)
            (Provider' ctx i (ProviderBase ctx i sh bh sf bf))
            eh
       , HFunctor sh
       )
    => i
    -> ( (Eff eh ef ~> Eff (sh ## tag ': ProviderFix ctx i sh bh sf bf ': bh) (sf # tag ': bf))
         -> Eff (sh ## tag ': ProviderFix ctx i sh bh sf bf ': bh) (sf # tag ': bf) a
       )
    -> Eff eh ef (ctx a)
scope i f =
    i ..! \runInBase ->
        ProviderBase . untag . untagH $ f $ tagH . tag . unProviderBase . runInBase

scope_
    :: forall tag i eh ef a sh bh sf bf
     . ( MemberHBy
            (ProviderKey Identity i)
            (Provider' Identity i (ProviderBase Identity i sh bh sf bf))
            eh
       , HFunctor sh
       )
    => i
    -> ( (Eff eh ef ~> Eff (sh ## tag ': ProviderFix_ i sh bh sf bf ': bh) (sf # tag ': bf))
         -> Eff (sh ## tag ': ProviderFix_ i sh bh sf bf ': bh) (sf # tag ': bf) a
       )
    -> Eff eh ef a
scope_ i f =
    i .! \runInBase ->
        ProviderBase . untag . untagH $ f $ tagH . tag . unProviderBase . runInBase
