-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2024 Sayo Koyoneda
License     :  MPL-2.0 (see the LICENSE file)
Maintainer  :  ymdfield@outlook.jp

Interpreters for the [Provider]("Data.Effect.Provider") effects.
-}
module Control.Monad.Hefty.Provider (
    module Control.Monad.Hefty.Provider,
    module Data.Effect.Provider,
)
where

import Control.Monad.Hefty (
    Eff,
    HFunctor,
    KeyH (KeyH),
    MemberHBy,
    Type,
    interpretH,
    key,
    keyH,
    transEffHF,
    unkey,
    unkeyH,
    weaken,
    weakenNH,
    type (##>),
    type (#>),
    type (~>),
 )
import Data.Effect.HFunctor (hfmap)
import Data.Effect.Provider
import Data.Functor.Const (Const (Const))
import Data.Functor.Identity (Identity (Identity))

type Provide ctx i sh sf eh ef = Provider ctx i (ProviderEff ctx i sh sf eh ef)

newtype ProviderEff ctx i sh sf eh ef p a
    = ProviderEff {unProviderEff :: Eff (sh p ': Provide ctx i sh sf eh ef ': eh) (sf p ': ef) a}

type Provide_ i sh sf eh ef =
    Provider (Const1 Identity) (Const i :: () -> Type) (Const1 (ProviderEff_ i sh sf eh ef))

newtype ProviderEff_ i sh sf eh ef a
    = ProviderEff_ {unProviderEff_ :: Eff (sh ': Provide_ i sh sf eh ef ': eh) (sf ': ef) a}

newtype Const2 ff x f a = Const2 {getConst2 :: ff f a}
instance (HFunctor ff) => HFunctor (Const2 ff x) where
    hfmap phi (Const2 ff) = Const2 $ hfmap phi ff

-- | Interpret the t'Provider' effect using the given effect interpreter.
runProvider
    :: forall ctx i sh sf eh ef
     . ( forall p x
          . i p
         -> Eff (sh p ': Provide ctx i sh sf eh ef ': eh) (sf p ': ef) x
         -> Eff (Provide ctx i sh sf eh ef ': eh) ef (ctx p x)
       )
    -> Eff (Provide ctx i sh sf eh ef ': eh) ef ~> Eff eh ef
runProvider run =
    interpretH \(KeyH (Provide i f)) ->
        runProvider run $
            run i (unProviderEff $ f $ ProviderEff . transEffHF (weakenNH @2) weaken)

{- |
Interpret the t'Provider' effect using the given effect interpreter.
A version of 'runProvider' where the type of t'Provider' is simpler.
-}
runProvider_
    :: forall i sh sf eh ef
     . (HFunctor sh)
    => ( forall x
          . i
         -> Eff (sh ': Provide_ i sh sf eh ef ': eh) (sf ': ef) x
         -> Eff (Provide_ i sh sf eh ef ': eh) ef x
       )
    -> Eff (Provide_ i sh sf eh ef ': eh) ef ~> Eff eh ef
runProvider_ run =
    interpretH \(KeyH (Provide (Const i) f)) ->
        runProvider_ run $
            run
                i
                ( fmap (Const1 . Identity)
                    . unProviderEff_
                    . getConst1
                    $ f
                    $ Const1
                        . ProviderEff_
                        . transEffHF (weakenNH @2) weaken
                )

-- | Introduces a new local scope that provides effects @sh p@ and @sf p@ parameterized by @i p@ value and with results wrapped in @ctx p@.
scope
    :: forall key ctx i p eh ef a sh sf bh bf
     . ( MemberHBy
            (ProviderKey ctx i)
            (Provider' ctx i (ProviderEff ctx i sh sf bh bf))
            eh
       , HFunctor (sh p)
       )
    => i p
    -> ( Eff eh ef ~> Eff (key ##> sh p ': Provide ctx i sh sf bh bf ': bh) (key #> sf p ': bf)
         -> Eff (key ##> sh p ': Provide ctx i sh sf bh bf ': bh) (key #> sf p ': bf) a
       )
    -> Eff eh ef (ctx p a)
scope i f =
    i ..! \runInScope ->
        ProviderEff $ unkeyH . unkey $ f (keyH . key . unProviderEff . runInScope)

-- | Introduces a new local scope that provides effects @sh@ and @sf@ parameterized by @i@ value.
scope_
    :: forall key i eh ef a sh sf bh bf
     . ( MemberHBy
            (ProviderKey (Const1 Identity :: () -> Type -> Type) (Const i :: () -> Type))
            ( Provider'
                (Const1 Identity)
                (Const i)
                (Const1 (ProviderEff_ i sh sf bh bf))
            )
            eh
       , HFunctor sh
       )
    => i
    -> ( Eff eh ef ~> Eff (key ##> sh ': Provide_ i sh sf bh bf ': bh) (key #> sf ': bf)
         -> Eff (key ##> sh ': Provide_ i sh sf bh bf ': bh) (key #> sf ': bf) a
       )
    -> Eff eh ef a
scope_ i f =
    i .! \runInScope ->
        ProviderEff_ $ unkeyH . unkey $ f (keyH . key . unProviderEff_ . runInScope)
