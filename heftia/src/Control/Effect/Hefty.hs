{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2023-2024 Yamada Ryo
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable

A Hefty carrier that can be used as a handler for effect systems based
on [@data-effects@](https://hackage.haskell.org/package/data-effects).
-}
module Control.Effect.Hefty where

import Control.Effect (type (~>))
import Control.Effect.Key (sendInsBy, sendSigBy)
import Control.Freer (Freer, InjectIns, InjectInsBy, injectIns, injectInsBy, interpretFreer, liftIns, transformFreer)
import Control.Hefty (Hefty (Hefty), InjectSig, InjectSigBy, injectSig, injectSigBy, overHefty, unHefty)
import Control.Monad.Cont (Cont, ContT (ContT), lift, runContT)
import Control.Monad.Freer (MonadFreer, interpretFreerK)
import Control.Monad.Identity (Identity (Identity), runIdentity)
import Control.Monad.Trans (MonadTrans)
import Data.Coerce (coerce)
import Data.Effect (LiftIns (LiftIns), Nop, SigClass, unliftIns)
import Data.Effect.HFunctor (HFunctor, caseH, hfmap, (:+:))
import Data.Effect.Key (Key (Key), KeyH (KeyH), unKey, unKeyH, type (##>), type (#>))
import Data.Effect.Tag (Tag (unTag), TagH (unTagH), type (#), type (##))
import Data.Free.Sum (caseF, pattern L1, pattern R1, type (+))
import Data.Hefty.Union (
    HFunctorUnion,
    HFunctorUnion_ (ForallHFunctor),
    HeadIns,
    LiftInsIfSingle (liftInsIfSingle, unliftInsIfSingle),
    Lookup,
    Member,
    MemberBy,
    MemberH,
    MemberHBy,
    MemberRec,
    U,
    UH,
    Union (HasMembership, exhaust, inject0, weaken, weakenUnder, (|+:)),
    UnliftIfSingle,
    flipUnion,
    flipUnion3,
    flipUnionUnder,
    inject,
    injectRec,
    projectRec,
    weaken2,
    weaken2Under,
    weaken2Under2,
    weaken3Under,
    weakenUnder2,
    weakenUnder3,
    (|+),
 )
import Data.Kind (Type)
import Data.Maybe.Singletons (FromJust)

{- |
A common type for representing first-order and higher-order extensible effectful programs that can
issue effects belonging to the specified list of effect classes.
-}
type Eff u fr ehs efs = Hefty fr (EffUnion u ehs efs)

{- |
A common type for representing first-order and higher-order extensible effectful programs that can
issue effects belonging to the specified sum of effect classes.
-}
type Effectful u fr eh ef = Eff u fr (UH u eh) (U u ef)

{- |
A common wrapper data type for representing first-order and higher-order extensible effect union.
-}
newtype EffUnion (u :: [SigClass] -> SigClass) ehs efs f a = EffUnion
    {unEffUnion :: (u ehs f + u efs Nop) a}

caseHF :: (u ehs f a -> r) -> (u efs Nop a -> r) -> EffUnion u ehs efs f a -> r
caseHF f g = caseF f g . unEffUnion

instance HFunctor (u ehs) => HFunctor (EffUnion u ehs efs) where
    hfmap f = EffUnion . caseF (L1 . hfmap f) R1 . unEffUnion
    {-# INLINE hfmap #-}

instance MemberRec u (LiftIns e) efs => InjectIns e (EffUnion u ehs efs f) where
    injectIns = EffUnion . R1 . injectRec . LiftIns
    {-# INLINE injectIns #-}

instance MemberRec u e ehs => InjectSig e (EffUnion u ehs efs) where
    injectSig = EffUnion . L1 . injectRec
    {-# INLINE injectSig #-}

type HasMembershipF u e efs = HasMembership u (LiftIns e) efs

infixr 3 $
infixr 4 $$

-- | Type-level infix applcation for functors.
type (f :: Type -> Type) $ a = f a

-- | Type-level infix applcation for higher-order functors.
type (h :: (Type -> Type) -> Type -> Type) $$ f = h f

type Elab e f = e f ~> f

injectH :: (Freer c f, HFunctor (u ehs)) => u ehs (Eff u f ehs efs) ~> Eff u f ehs efs
injectH = Hefty . liftIns . EffUnion . L1
{-# INLINE injectH #-}

injectF :: Freer c f => u efs Nop ~> Eff u f ehs efs
injectF = Hefty . liftIns . EffUnion . R1
{-# INLINE injectF #-}

{-  all types of interpret-family functions:
        - interpret   :                 e  ~> E r           ->    E (e + r)  ~> E r
        - reinterpret :                 e1 ~> E (e2 + r)    ->    E (e1 + r) ~> E (e2 + r)
        - interpose   :  e <| es  =>    e  ~> E es          ->    E es       ~> E es

        all possible suffix patterns of interpret functions:
            { <none> , K , ContT , Fin , T } x { <none> , H , FH }
            - Rec
            - RecH
            - RecFH

        all possible suffix patterns of interpret-family functions (except 'interpret'):
            - <none>
            - K
            - ContT
            - Fin ('interpose' only)
            - T
            - Rec
            - RecH
            - RecFH

    all types of transform-family functions:
        - transform :                  e1 ~> e2    ->    E (e1 + r) ~> E (e2 + r)
        - translate :  e2 <| r   =>    e1 ~> e2    ->    E (e1 + r) ~> E r
        - rewrite   :  e  <| es  =>    e  ~> e     ->    E es       ~> E es

        all possible suffix patterns of transform-family functions:
            - <none>
            - H
            - FH

    todo patterns:
        - *FH in interpret-family ( (5+1) + 2 = 8 functions )

        + *By (for keyed effects) in interpose/translate/rewrite ( 8 + 2x3 = 14 functions )
-}

-- | Using the provided interpretation function, interpret first-order effects.
interpret ::
    forall e r ehs fr u c.
    (Freer c fr, Union u, HeadIns e) =>
    UnliftIfSingle e ~> Eff u fr ehs r ->
    Eff u fr '[] (e ': r) ~> Eff u fr ehs r
interpret i = interpretAllE $ i . unliftInsIfSingle |+: injectF
{-# INLINE interpret #-}

interpretH ::
    forall eh ehs efs fr u c.
    (Freer c fr, Union u) =>
    eh (Eff u fr '[eh] efs) ~> Eff u fr ehs efs ->
    Eff u fr '[eh] efs ~> Eff u fr ehs efs
interpretH i = interpretAllH $ i |+: exhaust
{-# INLINE interpretH #-}

-- | Interpret the leading first-order effect class using delimited continuations.
interpretK ::
    forall e rs r a ehs fr u c.
    (MonadFreer c fr, Union u, HeadIns e, c (Eff u fr ehs rs)) =>
    (a -> Eff u fr ehs rs r) ->
    (forall x. (x -> Eff u fr ehs rs r) -> UnliftIfSingle e x -> Eff u fr ehs rs r) ->
    Eff u fr '[] (e ': rs) a ->
    Eff u fr ehs rs r
interpretK = toInterpretKFromContT interpretContT
{-# INLINE interpretK #-}

interpretKH ::
    forall e r ehs efs a fr u c.
    (MonadFreer c fr, Union u, c (Eff u fr ehs efs)) =>
    (a -> Eff u fr ehs efs r) ->
    (forall x. (x -> Eff u fr ehs efs r) -> e (Eff u fr '[e] efs) x -> Eff u fr ehs efs r) ->
    Eff u fr '[e] efs a ->
    Eff u fr ehs efs r
interpretKH = toInterpretKFromContT interpretContTH
{-# INLINE interpretKH #-}

-- | Interpret the leading first-order effect class using a continuation monad transformer.
interpretContT ::
    forall e rs r ehs fr u c.
    (MonadFreer c fr, Union u, HeadIns e, c (Eff u fr ehs rs)) =>
    (UnliftIfSingle e ~> ContT r (Eff u fr ehs rs)) ->
    Eff u fr '[] (e ': rs) ~> ContT r (Eff u fr ehs rs)
interpretContT i =
    interpretContTAll $ i . unliftInsIfSingle |+: lift . injectF
{-# INLINE interpretContT #-}

interpretContTH ::
    forall e r ehs efs fr u c.
    (MonadFreer c fr, Union u, c (Eff u fr ehs efs)) =>
    (e (Eff u fr '[e] efs) ~> ContT r (Eff u fr ehs efs)) ->
    Eff u fr '[e] efs ~> ContT r (Eff u fr ehs efs)
interpretContTH i = interpretContTAllH $ i |+: exhaust
{-# INLINE interpretContTH #-}

-- | Interpret the leading first-order effect class into the carrier @f@.
interpretFin ::
    forall e r f fr u c.
    (Freer c fr, Union u, HeadIns e, c f) =>
    (u r Nop ~> f) ->
    UnliftIfSingle e ~> f ->
    Eff u fr '[] (e ': r) ~> f
interpretFin liftFin i = interpretAll $ i . unliftInsIfSingle |+: liftFin
{-# INLINE interpretFin #-}

interpretFinH ::
    forall e f efs fr u c.
    (Freer c fr, Union u, c f) =>
    (u efs Nop ~> f) ->
    e (Eff u fr '[e] efs) ~> f ->
    Eff u fr '[e] efs ~> f
interpretFinH liftFin i = interpretAllFH (i |+: exhaust) liftFin
{-# INLINE interpretFinH #-}

-- | Interpret the leading first-order effect class using a monad transformer.
interpretT ::
    forall e r t ehs fr u c.
    ( Freer c fr
    , Union u
    , MonadTrans t
    , HeadIns e
    , Monad (Eff u fr ehs r)
    , c (t (Eff u fr ehs r))
    ) =>
    UnliftIfSingle e ~> t (Eff u fr ehs r) ->
    Eff u fr '[] (e ': r) ~> t (Eff u fr ehs r)
interpretT = interpretFin $ lift . injectF
{-# INLINE interpretT #-}

interpretTH ::
    forall e t ehs efs fr u c.
    (Freer c fr, Union u, MonadTrans t, Monad (Eff u fr ehs efs), c (t (Eff u fr ehs efs))) =>
    e (Eff u fr '[e] efs) ~> t (Eff u fr ehs efs) ->
    Eff u fr '[e] efs ~> t (Eff u fr ehs efs)
interpretTH = interpretFinH $ lift . injectF
{-# INLINE interpretTH #-}

{- |
Using the provided interpretation function, interpret first-order effects. For actions (scopes)
within higher-order effects that are currently unhandled, interpretation is applied recursively.

Note that if the interpretation function is stateful (i.e., not a monad morphism), the state is not
maintained across the scopes.
-}
interpretRec ::
    forall e rs ehs fr u c.
    (Freer c fr, Union u, HFunctor (u ehs), HeadIns e) =>
    UnliftIfSingle e ~> Eff u fr ehs rs ->
    Eff u fr ehs (e ': rs) ~> Eff u fr ehs rs
interpretRec i = interpretAllRec $ i . unliftInsIfSingle |+: injectF
{-# INLINE interpretRec #-}

{- |
Using the provided interpretation function, interpret higher-order effects. For actions (scopes)
within higher-order effects that are currently unhandled, interpretation is applied recursively.

Note that if the interpretation function is stateful (i.e., not a monad morphism), the state is not
maintained across the scopes.
-}
interpretRecH ::
    forall e rs efs fr u c.
    (Freer c fr, Union u, HFunctor e, HFunctor (u rs), HFunctor (u (e ': rs))) =>
    e (Eff u fr rs efs) ~> Eff u fr rs efs ->
    Eff u fr (e ': rs) efs ~> Eff u fr rs efs
interpretRecH i = interpretAllRecH $ i |+: injectH
{-# INLINE interpretRecH #-}

reinterpret ::
    forall e2 e1 r ehs fr u c.
    (Freer c fr, Union u, HeadIns e1, HFunctor (u '[])) =>
    UnliftIfSingle e1 ~> Eff u fr ehs (e2 ': r) ->
    Eff u fr '[] (e1 ': r) ~> Eff u fr ehs (e2 ': r)
reinterpret f = interpret f . raiseUnder
{-# INLINE reinterpret #-}

reinterpretK ::
    forall e2 e1 rs r a ehs fr u c.
    (MonadFreer c fr, Union u, HeadIns e1, HFunctor (u '[]), c (Eff u fr ehs (e2 ': rs))) =>
    (a -> Eff u fr ehs (e2 ': rs) r) ->
    ( forall x.
      (x -> Eff u fr ehs (e2 ': rs) r) ->
      UnliftIfSingle e1 x ->
      Eff u fr ehs (e2 ': rs) r
    ) ->
    Eff u fr '[] (e1 ': rs) a ->
    Eff u fr ehs (e2 ': rs) r
reinterpretK = toInterpretKFromContT reinterpretContT
{-# INLINE reinterpretK #-}

reinterpretContT ::
    forall e2 e1 rs r ehs fr u c.
    (MonadFreer c fr, Union u, HeadIns e1, HFunctor (u '[]), c (Eff u fr ehs (e2 ': rs))) =>
    (UnliftIfSingle e1 ~> ContT r (Eff u fr ehs (e2 ': rs))) ->
    Eff u fr '[] (e1 ': rs) ~> ContT r (Eff u fr ehs (e2 ': rs))
reinterpretContT i = interpretContT i . raiseUnder
{-# INLINE reinterpretContT #-}

reinterpretT ::
    forall e2 e1 t r ehs fr u c.
    ( Freer c fr
    , Union u
    , MonadTrans t
    , HeadIns e1
    , HFunctor (u '[])
    , Monad (Eff u fr ehs (e2 ': r))
    , c (t (Eff u fr ehs (e2 ': r)))
    ) =>
    UnliftIfSingle e1 ~> t (Eff u fr ehs (e2 ': r)) ->
    Eff u fr '[] (e1 ': r) ~> t (Eff u fr ehs (e2 ': r))
reinterpretT i = interpretT i . raiseUnder
{-# INLINE reinterpretT #-}

reinterpretRec ::
    forall e2 e1 r ehs fr u c.
    (Freer c fr, Union u, HFunctor (u ehs), HeadIns e1) =>
    UnliftIfSingle e1 ~> Eff u fr ehs (e2 ': r) ->
    Eff u fr ehs (e1 ': r) ~> Eff u fr ehs (e2 ': r)
reinterpretRec i = interpretRec i . raiseUnder
{-# INLINE reinterpretRec #-}

reinterpretRecH ::
    forall e2 e1 r efs fr u c.
    (Freer c fr, HFunctorUnion u, HFunctor e1, HFunctor e2, ForallHFunctor u r) =>
    e1 (Eff u fr (e2 ': r) efs) ~> Eff u fr (e2 ': r) efs ->
    Eff u fr (e1 ': r) efs ~> Eff u fr (e2 ': r) efs
reinterpretRecH i = interpretRecH i . raiseUnderH
{-# INLINE reinterpretRecH #-}

interpose ::
    forall e efs fr u c.
    (Freer c fr, Union u, Member u e efs) =>
    e ~> Eff u fr '[] efs ->
    Eff u fr '[] efs ~> Eff u fr '[] efs
interpose f =
    interpretAllE
        \u -> case projectRec u of
            Just (LiftIns e) -> f e
            Nothing -> injectF u

interposeK ::
    forall e efs r a fr u c.
    (MonadFreer c fr, Union u, Member u e efs, c (Eff u fr '[] efs)) =>
    (a -> Eff u fr '[] efs r) ->
    (forall x. (x -> Eff u fr '[] efs r) -> e x -> Eff u fr '[] efs r) ->
    Eff u fr '[] efs a ->
    Eff u fr '[] efs r
interposeK = toInterpretKFromContT interposeContT
{-# INLINE interposeK #-}

interposeContT ::
    forall e efs r fr u c.
    (MonadFreer c fr, Union u, Member u e efs, c (Eff u fr '[] efs)) =>
    (e ~> ContT r (Eff u fr '[] efs)) ->
    Eff u fr '[] efs ~> ContT r (Eff u fr '[] efs)
interposeContT f =
    interpretContTAll
        \u -> case projectRec u of
            Just (LiftIns e) -> f e
            Nothing -> lift $ injectF u
{-# INLINE interposeContT #-}

interposeFin ::
    forall e f efs fr u c.
    (Freer c fr, Union u, Member u e efs, c f) =>
    u efs Nop ~> f ->
    e ~> f ->
    Eff u fr '[] efs ~> f
interposeFin liftFin f =
    interpretAll
        \u -> case projectRec u of
            Just (LiftIns e) -> f e
            Nothing -> liftFin u
{-# INLINE interposeFin #-}

interposeT ::
    forall e t efs fr u c.
    ( Freer c fr
    , Union u
    , MonadTrans t
    , Member u e efs
    , Monad (Eff u fr '[] efs)
    , c (t (Eff u fr '[] efs))
    ) =>
    e ~> t (Eff u fr '[] efs) ->
    Eff u fr '[] efs ~> t (Eff u fr '[] efs)
interposeT = interposeFin $ lift . injectF
{-# INLINE interposeT #-}

interposeRec ::
    forall e ehs efs fr u c.
    (Freer c fr, Union u, HFunctor (u ehs), Member u e efs) =>
    e ~> Eff u fr ehs efs ->
    Eff u fr ehs efs ~> Eff u fr ehs efs
interposeRec f =
    interpretAllRec
        \u -> case projectRec u of
            Just (LiftIns e) -> f e
            Nothing -> injectF u
{-# INLINE interposeRec #-}

interposeRecH ::
    forall e ehs efs fr u c.
    (Freer c fr, HFunctorUnion u, HFunctor e, ForallHFunctor u ehs, MemberH u e ehs) =>
    e (Eff u fr ehs efs) ~> Eff u fr ehs efs ->
    Eff u fr ehs efs ~> Eff u fr ehs efs
interposeRecH f =
    interpretAllRecH
        \u -> case projectRec u of
            Just e -> f e
            Nothing -> injectH u
{-# INLINE interposeRecH #-}

interpretAll ::
    forall g efs fr u c.
    (Freer c fr, Union u, c g) =>
    u efs Nop ~> g ->
    Eff u fr '[] efs ~> g
interpretAll = interpretAllFH exhaust
{-# INLINE interpretAll #-}

interpretAllE ::
    forall ehs' efs' efs fr u c.
    (Freer c fr, Union u) =>
    u efs Nop ~> Eff u fr ehs' efs' ->
    Eff u fr '[] efs ~> Eff u fr ehs' efs'
interpretAllE = interpretAllFHE exhaust
{-# INLINE interpretAllE #-}

interpretAllRecH ::
    forall ehs' ehs efs fr u c.
    (Freer c fr, Union u, HFunctor (u ehs)) =>
    u ehs (Eff u fr ehs' efs) ~> Eff u fr ehs' efs ->
    Eff u fr ehs efs ~> Eff u fr ehs' efs
interpretAllRecH fh =
    interpretAllH $ fh . hfmap (interpretAllRecH fh)

interpretAllH ::
    forall ehs' ehs efs fr u c.
    (Freer c fr, Union u) =>
    u ehs (Eff u fr ehs efs) ~> Eff u fr ehs' efs ->
    Eff u fr ehs efs ~> Eff u fr ehs' efs
interpretAllH fh = interpretAllFHE fh injectF
{-# INLINE interpretAllH #-}

interpretAllRecFH ::
    forall g ehs efs fr u c.
    (Freer c fr, Union u, c g, HFunctor (u ehs)) =>
    u ehs g ~> g ->
    u efs Nop ~> g ->
    Eff u fr ehs efs ~> g
interpretAllRecFH fh ff =
    interpretAllFH (fh . hfmap (interpretAllRecFH fh ff)) ff

interpretAllFH ::
    forall g ehs efs fr u c.
    (Freer c fr, Union u, c g) =>
    u ehs (Eff u fr ehs efs) ~> g ->
    u efs Nop ~> g ->
    Eff u fr ehs efs ~> g
interpretAllFH fh ff = interpretFreer (caseHF fh ff) . unHefty
{-# INLINE interpretAllFH #-}

interpretAllRecFHE ::
    forall ehs' efs' ehs efs fr u c.
    (Freer c fr, Union u, HFunctor (u ehs)) =>
    u ehs (Eff u fr ehs' efs') ~> Eff u fr ehs' efs' ->
    u efs Nop ~> Eff u fr ehs' efs' ->
    Eff u fr ehs efs ~> Eff u fr ehs' efs'
interpretAllRecFHE fh ff =
    interpretAllFHE (fh . hfmap (interpretAllRecFHE fh ff)) ff
{-# INLINE interpretAllRecFHE #-}

interpretAllFHE ::
    forall ehs' efs' ehs efs fr u c.
    (Freer c fr, Union u) =>
    u ehs (Eff u fr ehs efs) ~> Eff u fr ehs' efs' ->
    u efs Nop ~> Eff u fr ehs' efs' ->
    Eff u fr ehs efs ~> Eff u fr ehs' efs'
interpretAllFHE fh ff =
    overHefty $ interpretFreer $ unHefty . caseHF fh ff

interpretKAll ::
    forall r a efs fr u c.
    (MonadFreer c fr, Union u) =>
    (a -> Eff u fr '[] efs r) ->
    (forall x. (x -> Eff u fr '[] efs r) -> u efs Nop x -> Eff u fr '[] efs r) ->
    Eff u fr '[] efs a ->
    Eff u fr '[] efs r
interpretKAll = toInterpretKFromContT interpretContTAll
{-# INLINE interpretKAll #-}

interpretKAllRecH ::
    forall ehs' r a ehs efs fr u c.
    (MonadFreer c fr, Union u, HFunctor (u ehs), c (Eff u fr ehs' efs)) =>
    (a -> Eff u fr ehs' efs r) ->
    ( forall x.
      (x -> Eff u fr ehs' efs r) ->
      u ehs (ContT r (Eff u fr ehs' efs)) x ->
      Eff u fr ehs' efs r
    ) ->
    Eff u fr ehs efs a ->
    Eff u fr ehs' efs r
interpretKAllRecH = toInterpretKFromContT interpretContTAllRecH
{-# INLINE interpretKAllRecH #-}

interpretKAllH ::
    forall ehs' r a ehs efs fr u c.
    (MonadFreer c fr, Union u, c (Eff u fr ehs' efs)) =>
    (a -> Eff u fr ehs' efs r) ->
    ( forall x.
      (x -> Eff u fr ehs' efs r) ->
      u ehs (Eff u fr ehs efs) x ->
      Eff u fr ehs' efs r
    ) ->
    Eff u fr ehs efs a ->
    Eff u fr ehs' efs r
interpretKAllH = toInterpretKFromContT interpretContTAllH
{-# INLINE interpretKAllH #-}

interpretKAllRecFH ::
    forall g r a ehs efs fr u c.
    (MonadFreer c fr, Union u, HFunctor (u ehs)) =>
    (a -> g r) ->
    (forall x. (x -> g r) -> u ehs (ContT r g) x -> g r) ->
    (forall x. (x -> g r) -> u efs Nop x -> g r) ->
    Eff u fr ehs efs a ->
    g r
interpretKAllRecFH = toInterpretKFromContT2 interpretContTAllRecFH
{-# INLINE interpretKAllRecFH #-}

interpretKAllFH ::
    forall g r a ehs efs fr u c.
    (MonadFreer c fr, Union u) =>
    (a -> g r) ->
    (forall x. (x -> g r) -> u ehs (Eff u fr ehs efs) x -> g r) ->
    (forall x. (x -> g r) -> u efs Nop x -> g r) ->
    Eff u fr ehs efs a ->
    g r
interpretKAllFH = toInterpretKFromContT2 interpretContTAllFH
{-# INLINE interpretKAllFH #-}

interpretContTAll ::
    forall g r efs fr u c.
    (MonadFreer c fr, Union u) =>
    u efs Nop ~> ContT r g ->
    Eff u fr '[] efs ~> ContT r g
interpretContTAll f =
    transCont
        . interpretFreerK (caseHF exhaust (detransContT . f))
        . unHefty

interpretContTAllRecH ::
    forall ehs' r ehs efs fr u c.
    (MonadFreer c fr, Union u, HFunctor (u ehs), c (Eff u fr ehs' efs)) =>
    u ehs (ContT r (Eff u fr ehs' efs)) ~> ContT r (Eff u fr ehs' efs) ->
    Eff u fr ehs efs ~> ContT r (Eff u fr ehs' efs)
interpretContTAllRecH fh = interpretContTAllRecFH fh (lift . injectF)
{-# INLINE interpretContTAllRecH #-}

interpretContTAllH ::
    forall ehs' r ehs efs fr u c.
    (MonadFreer c fr, Union u, c (Eff u fr ehs' efs)) =>
    u ehs (Eff u fr ehs efs) ~> ContT r (Eff u fr ehs' efs) ->
    Eff u fr ehs efs ~> ContT r (Eff u fr ehs' efs)
interpretContTAllH fh = interpretContTAllFH fh (lift . injectF)
{-# INLINE interpretContTAllH #-}

interpretContTAllRecFH ::
    forall g r ehs efs fr u c.
    (MonadFreer c fr, Union u, HFunctor (u ehs)) =>
    u ehs (ContT r g) ~> ContT r g ->
    u efs Nop ~> ContT r g ->
    Eff u fr ehs efs ~> ContT r g
interpretContTAllRecFH fh ff =
    transCont
        . interpretFreerK (detransContT . caseHF (fh . hfmap (interpretContTAllRecFH fh ff)) ff)
        . unHefty

interpretContTAllFH ::
    forall g r ehs efs fr u c.
    (MonadFreer c fr, Union u) =>
    u ehs (Eff u fr ehs efs) ~> ContT r g ->
    u efs Nop ~> ContT r g ->
    Eff u fr ehs efs ~> ContT r g
interpretContTAllFH fh ff =
    transCont
        . interpretFreerK (detransContT . caseHF fh ff)
        . unHefty

transCont :: Cont (m r) ~> ContT r m
transCont (ContT f) = ContT \k -> coerce $ f $ coerce . k
{-# INLINE transCont #-}

detransContT :: ContT r m ~> Cont (m r)
detransContT (ContT f) = ContT \k -> coerce $ f $ coerce . k
{-# INLINE detransContT #-}

toInterpretKFromContT ::
    ((e ~> ContT r m) -> f ~> ContT r m') ->
    (a -> m' r) ->
    (forall x. (x -> m r) -> e x -> m r) ->
    f a ->
    m' r
toInterpretKFromContT intContT k i = (`runContT` k) . intContT \e -> ContT (`i` e)
{-# INLINE toInterpretKFromContT #-}

toInterpretKFromContT2 ::
    ((e1 ~> ContT r m) -> (e2 ~> ContT r m) -> f ~> ContT r m') ->
    (a -> m' r) ->
    (forall x. (x -> m r) -> e1 x -> m r) ->
    (forall x. (x -> m r) -> e2 x -> m r) ->
    f a ->
    m' r
toInterpretKFromContT2 intContT k i1 i2 =
    (`runContT` k) . intContT (\e -> ContT (`i1` e)) (\e -> ContT (`i2` e))
{-# INLINE toInterpretKFromContT2 #-}

interpretTAll ::
    forall t g efs fr u c.
    (Freer c fr, Union u, c (t g)) =>
    u efs Nop ~> t g ->
    Eff u fr '[] efs ~> t g
interpretTAll = interpretAll
{-# INLINE interpretTAll #-}

interpretTAllRecH ::
    forall ehs' t ehs efs fr u c.
    ( Freer c fr
    , Union u
    , MonadTrans t
    , HFunctor (u ehs)
    , Monad (Eff u fr ehs' efs)
    , c (t (Eff u fr ehs' efs))
    ) =>
    u ehs (t (Eff u fr ehs' efs)) ~> t (Eff u fr ehs' efs) ->
    Eff u fr ehs efs ~> t (Eff u fr ehs' efs)
interpretTAllRecH i = interpretAllRecFH i (lift . injectF)
{-# INLINE interpretTAllRecH #-}

interpretTAllH ::
    forall ehs' t ehs efs fr u c.
    (Freer c fr, Union u, MonadTrans t, Monad (Eff u fr ehs' efs), c (t (Eff u fr ehs' efs))) =>
    u ehs (Eff u fr ehs efs) ~> t (Eff u fr ehs' efs) ->
    Eff u fr ehs efs ~> t (Eff u fr ehs' efs)
interpretTAllH i = interpretAllFH i (lift . injectF)
{-# INLINE interpretTAllH #-}

interpretAllRec ::
    forall efs' ehs efs fr u c.
    (Freer c fr, Union u, HFunctor (u ehs)) =>
    u efs Nop ~> Eff u fr ehs efs' ->
    Eff u fr ehs efs ~> Eff u fr ehs efs'
interpretAllRec = interpretAllRecFHE injectH
{-# INLINE interpretAllRec #-}

transform ::
    forall e2 e1 r ehs fr u c.
    (Freer c fr, Union u, HFunctor (u ehs), HeadIns e1, HeadIns e2) =>
    (UnliftIfSingle e1 ~> UnliftIfSingle e2) ->
    Eff u fr ehs (e1 ': r) ~> Eff u fr ehs (e2 ': r)
transform f =
    transformAll $
        inject0 . liftInsIfSingle . f . unliftInsIfSingle |+: weaken
{-# INLINE transform #-}

transformH ::
    forall e2 e1 r efs fr u c.
    (Freer c fr, Union u, HFunctor (u (e1 ': r))) =>
    (e1 (Eff u fr (e2 ': r) efs) ~> e2 (Eff u fr (e2 ': r) efs)) ->
    Eff u fr (e1 ': r) efs ~> Eff u fr (e2 ': r) efs
transformH f = transformAllH $ inject0 . f |+: weaken
{-# INLINE transformH #-}

transformFH ::
    forall e2h e2f e1h e1f rh rf fr u c.
    (Freer c fr, Union u, HFunctor (u (e1h ': rh)), HeadIns e1f, HeadIns e2f) =>
    (e1h (Eff u fr (e2h ': rh) (e2f ': rf)) ~> e2h (Eff u fr (e2h ': rh) (e2f ': rf))) ->
    (UnliftIfSingle e1f ~> UnliftIfSingle e2f) ->
    Eff u fr (e1h ': rh) (e1f ': rf) ~> Eff u fr (e2h ': rh) (e2f ': rf)
transformFH fh ff =
    transformAllFH
        (inject0 . fh |+: weaken)
        (inject0 . liftInsIfSingle . ff . unliftInsIfSingle |+: weaken)
{-# INLINE transformFH #-}

translate ::
    forall e2 e1 es ehs fr u c.
    (Freer c fr, Union u, Member u e2 es, HFunctor (u ehs), HeadIns e1) =>
    (UnliftIfSingle e1 ~> e2) ->
    Eff u fr ehs (e1 ': es) ~> Eff u fr ehs es
translate f =
    transformAll $
        injectRec . LiftIns . f . unliftInsIfSingle |+: id
{-# INLINE translate #-}

translateH ::
    forall e2 e1 es efs fr u c.
    (Freer c fr, Union u, MemberH u e2 es, HFunctor (u (e1 ': es))) =>
    (e1 (Eff u fr es efs) ~> e2 (Eff u fr es efs)) ->
    Eff u fr (e1 ': es) efs ~> Eff u fr es efs
translateH f = transformAllH $ injectRec . f |+: id
{-# INLINE translateH #-}

translateFH ::
    forall e2h e2f e1h e1f ehs efs fr u c.
    ( Freer c fr
    , Union u
    , MemberH u e2h ehs
    , Member u e2f efs
    , HFunctor (u (e1h ': ehs))
    , HeadIns e1f
    ) =>
    (e1h (Eff u fr ehs efs) ~> e2h (Eff u fr ehs efs)) ->
    (UnliftIfSingle e1f ~> e2f) ->
    Eff u fr (e1h ': ehs) (e1f ': efs) ~> Eff u fr ehs efs
translateFH fh ff =
    transformAllFH
        (injectRec . fh |+: id)
        (injectRec . LiftIns . ff . unliftInsIfSingle |+: id)
{-# INLINE translateFH #-}

rewrite ::
    forall e efs ehs fr u c.
    (Freer c fr, Union u, HFunctor (u ehs), Member u e efs) =>
    (e ~> e) ->
    Eff u fr ehs efs ~> Eff u fr ehs efs
rewrite f =
    transformAll
        \u -> case projectRec u of
            Just (LiftIns e) -> injectRec $ LiftIns $ f e
            Nothing -> u
{-# INLINE rewrite #-}

rewriteH ::
    forall e efs ehs fr u c.
    (Freer c fr, Union u, HFunctor (u ehs), MemberH u e ehs) =>
    (e (Eff u fr ehs efs) ~> e (Eff u fr ehs efs)) ->
    Eff u fr ehs efs ~> Eff u fr ehs efs
rewriteH f =
    transformAllH
        \u -> case projectRec u of
            Just e -> injectRec $ f e
            Nothing -> u
{-# INLINE rewriteH #-}

rewriteFH ::
    forall eh ef efs ehs fr u c.
    (Freer c fr, Union u, HFunctor (u ehs), MemberH u eh ehs, Member u ef efs) =>
    (eh (Eff u fr ehs efs) ~> eh (Eff u fr ehs efs)) ->
    (ef ~> ef) ->
    Eff u fr ehs efs ~> Eff u fr ehs efs
rewriteFH fh ff =
    transformAllFH
        ( \u -> case projectRec u of
            Just e -> injectRec $ fh e
            Nothing -> u
        )
        ( \u -> case projectRec u of
            Just (LiftIns e) -> injectRec $ LiftIns $ ff e
            Nothing -> u
        )
{-# INLINE rewriteFH #-}

transformAll ::
    forall efs' efs ehs fr u c.
    (Freer c fr, Union u, HFunctor (u ehs)) =>
    u efs Nop ~> u efs' Nop ->
    Eff u fr ehs efs ~> Eff u fr ehs efs'
transformAll = transformAllFH id
{-# INLINE transformAll #-}

transformAllH ::
    forall ehs' ehs efs fr u c.
    (Freer c fr, Union u, HFunctor (u ehs)) =>
    u ehs (Eff u fr ehs' efs) ~> u ehs' (Eff u fr ehs' efs) ->
    Eff u fr ehs efs ~> Eff u fr ehs' efs
transformAllH f = transformAllFH f id
{-# INLINE transformAllH #-}

transformAllFH ::
    forall ehs' efs' ehs efs fr u c.
    (Freer c fr, Union u, HFunctor (u ehs)) =>
    u ehs (Eff u fr ehs' efs') ~> u ehs' (Eff u fr ehs' efs') ->
    (u efs Nop ~> u efs' Nop) ->
    Eff u fr ehs efs ~> Eff u fr ehs' efs'
transformAllFH fh ff =
    overHefty $
        transformFreer $
            EffUnion
                . caseHF
                    (L1 . fh . hfmap (transformAllFH fh ff))
                    (R1 . ff)

raise ::
    forall e r ehs fr u c.
    (Freer c fr, Union u, HFunctor (u ehs)) =>
    Eff u fr ehs r ~> Eff u fr ehs (e ': r)
raise = transformAll weaken
{-# INLINE raise #-}

raiseH ::
    forall e r efs fr u c.
    (Freer c fr, Union u, HFunctor (u r)) =>
    Eff u fr r efs ~> Eff u fr (e ': r) efs
raiseH = transformAllH weaken
{-# INLINE raiseH #-}

raise2H ::
    forall e2 e1 r efs fr u c.
    (Freer c fr, Union u, HFunctor (u r)) =>
    Eff u fr r efs ~> Eff u fr (e2 ': e1 ': r) efs
raise2H = transformAllH weaken2
{-# INLINE raise2H #-}

raiseUnder ::
    forall e1 e2 r ehs fr u c.
    (Freer c fr, Union u, HFunctor (u ehs)) =>
    Eff u fr ehs (e2 ': r) ~> Eff u fr ehs (e2 ': e1 ': r)
raiseUnder = transformAll weakenUnder
{-# INLINE raiseUnder #-}

raiseUnder2 ::
    forall e1 e2 e3 r ehs fr u c.
    (Freer c fr, Union u, HFunctor (u ehs)) =>
    Eff u fr ehs (e3 ': e2 ': r) ~> Eff u fr ehs (e3 ': e2 ': e1 ': r)
raiseUnder2 = transformAll weakenUnder2
{-# INLINE raiseUnder2 #-}

raiseUnder3 ::
    forall e1 e2 e3 e4 r ehs fr u c.
    (Freer c fr, Union u, HFunctor (u ehs)) =>
    Eff u fr ehs (e4 ': e3 ': e2 ': r) ~> Eff u fr ehs (e4 ': e3 ': e2 ': e1 ': r)
raiseUnder3 = transformAll weakenUnder3
{-# INLINE raiseUnder3 #-}

raise2Under2 ::
    forall e1 e2 e3 e4 r ehs fr u c.
    (Freer c fr, Union u, HFunctor (u ehs)) =>
    Eff u fr ehs (e4 ': e3 ': r) ~> Eff u fr ehs (e4 ': e3 ': e2 ': e1 ': r)
raise2Under2 = transformAll weaken2Under2
{-# INLINE raise2Under2 #-}

raise2Under ::
    forall e1 e2 e3 r ehs fr u c.
    (Freer c fr, Union u, HFunctor (u ehs)) =>
    Eff u fr ehs (e3 ': r) ~> Eff u fr ehs (e3 ': e2 ': e1 ': r)
raise2Under = transformAll weaken2Under
{-# INLINE raise2Under #-}

raise3Under ::
    forall e1 e2 e3 e4 r ehs fr u c.
    (Freer c fr, Union u, HFunctor (u ehs)) =>
    Eff u fr ehs (e4 ': r) ~> Eff u fr ehs (e4 ': e3 ': e2 ': e1 ': r)
raise3Under = transformAll weaken3Under
{-# INLINE raise3Under #-}

raiseUnderH ::
    forall e1 e2 r efs fr u c.
    (Freer c fr, Union u, HFunctor (u (e2 ': r))) =>
    Eff u fr (e2 ': r) efs ~> Eff u fr (e2 ': e1 ': r) efs
raiseUnderH = transformAllH weakenUnder
{-# INLINE raiseUnderH #-}

raiseUnder2H ::
    forall e1 e2 e3 r efs fr u c.
    (Freer c fr, Union u, HFunctor (u (e3 ': e2 ': r))) =>
    Eff u fr (e3 ': e2 ': r) efs ~> Eff u fr (e3 ': e2 ': e1 ': r) efs
raiseUnder2H = transformAllH weakenUnder2
{-# INLINE raiseUnder2H #-}

raiseUnder3H ::
    forall e1 e2 e3 e4 r efs fr u c.
    (Freer c fr, Union u, HFunctor (u (e4 ': e3 ': e2 ': r))) =>
    Eff u fr (e4 ': e3 ': e2 ': r) efs ~> Eff u fr (e4 ': e3 ': e2 ': e1 ': r) efs
raiseUnder3H = transformAllH weakenUnder3
{-# INLINE raiseUnder3H #-}

raise2Under2H ::
    forall e1 e2 e3 e4 r efs fr u c.
    (Freer c fr, Union u, HFunctor (u (e4 ': e3 ': r))) =>
    Eff u fr (e4 ': e3 ': r) efs ~> Eff u fr (e4 ': e3 ': e2 ': e1 ': r) efs
raise2Under2H = transformAllH weaken2Under2
{-# INLINE raise2Under2H #-}

raise2UnderH ::
    forall e1 e2 e3 r efs fr u c.
    (Freer c fr, Union u, HFunctor (u (e3 ': r))) =>
    Eff u fr (e3 ': r) efs ~> Eff u fr (e3 ': e2 ': e1 ': r) efs
raise2UnderH = transformAllH weaken2Under
{-# INLINE raise2UnderH #-}

raise3UnderH ::
    forall e1 e2 e3 e4 r efs fr u c.
    (Freer c fr, Union u, HFunctor (u (e4 ': r))) =>
    Eff u fr (e4 ': r) efs ~> Eff u fr (e4 ': e3 ': e2 ': e1 ': r) efs
raise3UnderH = transformAllH weaken3Under
{-# INLINE raise3UnderH #-}

raiseAll ::
    forall ehs efs fr u c.
    (Freer c fr, Union u, HFunctor (u ehs)) =>
    Eff u fr ehs '[] ~> Eff u fr ehs efs
raiseAll = transformAll exhaust
{-# INLINE raiseAll #-}

raiseAllH ::
    forall ehs efs fr u c.
    (Freer c fr, Union u) =>
    Eff u fr '[] efs ~> Eff u fr ehs efs
raiseAllH = overHefty $ transformFreer $ EffUnion . caseHF exhaust R1
{-# INLINE raiseAllH #-}

flipEff ::
    forall e1 e2 r ehs fr u c.
    (Freer c fr, Union u, HFunctor (u ehs)) =>
    Eff u fr ehs (e1 ': e2 ': r) ~> Eff u fr ehs (e2 ': e1 ': r)
flipEff = transformAll flipUnion
{-# INLINE flipEff #-}

flipEff3 ::
    forall e1 e2 e3 r ehs fr u c.
    (Freer c fr, Union u, HFunctor (u ehs)) =>
    Eff u fr ehs (e1 ': e2 ': e3 ': r) ~> Eff u fr ehs (e3 ': e2 ': e1 ': r)
flipEff3 = transformAll flipUnion3
{-# INLINE flipEff3 #-}

flipEffUnder ::
    forall e1 e2 e3 r ehs fr u c.
    (Freer c fr, Union u, HFunctor (u ehs)) =>
    Eff u fr ehs (e3 ': e1 ': e2 ': r) ~> Eff u fr ehs (e3 ': e2 ': e1 ': r)
flipEffUnder = transformAll flipUnionUnder
{-# INLINE flipEffUnder #-}

flipEffH ::
    forall e1 e2 r efs fr u c.
    (Freer c fr, Union u, HFunctor (u (e1 ': e2 ': r))) =>
    Eff u fr (e1 ': e2 ': r) efs ~> Eff u fr (e2 ': e1 ': r) efs
flipEffH = transformAllH flipUnion
{-# INLINE flipEffH #-}

flipEff3H ::
    forall e1 e2 e3 r efs fr u c.
    (Freer c fr, Union u, HFunctor (u (e1 ': e2 ': e3 ': r))) =>
    Eff u fr (e1 ': e2 ': e3 ': r) efs ~> Eff u fr (e3 ': e2 ': e1 ': r) efs
flipEff3H = transformAllH flipUnion3
{-# INLINE flipEff3H #-}

flipEffUnderH ::
    forall e1 e2 e3 r efs fr u c.
    (Freer c fr, Union u, HFunctor (u (e3 ': e1 ': e2 ': r))) =>
    Eff u fr (e3 ': e1 ': e2 ': r) efs ~> Eff u fr (e3 ': e2 ': e1 ': r) efs
flipEffUnderH = transformAllH flipUnionUnder
{-# INLINE flipEffUnderH #-}

subsume ::
    forall e r ehs fr u c.
    (Freer c fr, Union u, HFunctor (u ehs), HasMembership u e r) =>
    Eff u fr ehs (e ': r) ~> Eff u fr ehs r
subsume = transformAll $ inject |+: id
{-# INLINE subsume #-}

subsumeH ::
    forall e r efs fr u c.
    (Freer c fr, Union u, HFunctor (u (e ': r)), HasMembership u e r) =>
    Eff u fr (e ': r) efs ~> Eff u fr r efs
subsumeH = transformAllH $ inject |+: id
{-# INLINE subsumeH #-}

liftInsEff ::
    forall e eh ef fr u c.
    (Freer c fr, Union u, HFunctor (u eh), HFunctor e) =>
    Eff u fr eh (e ': ef) ~> Eff u fr (e ': eh) ef
liftInsEff =
    overHefty $
        transformFreer $
            EffUnion
                . caseHF
                    (L1 . weaken . hfmap liftInsEff)
                    (L1 . inject0 . hfmap \case {} |+: R1)

splitEff ::
    forall fr' e r ehs fr u c.
    (Freer c fr', Freer c fr, Union u, HeadIns e) =>
    Eff u fr '[] (e ': r) ~> fr' (UnliftIfSingle e + Eff u fr ehs r)
splitEff = interpretAll $ liftIns . (L1 . unliftInsIfSingle |+: R1 . injectF)
{-# INLINE splitEff #-}

mergeEff ::
    forall fr' e r ehs fr u c.
    (Freer c fr', Freer c fr, Union u, HeadIns e, c (Eff u fr ehs (e ': r)), HFunctor (u ehs)) =>
    fr' (UnliftIfSingle e + Eff u fr ehs r) ~> Eff u fr ehs (e ': r)
mergeEff = interpretFreer $ caseF send0 raise
{-# INLINE mergeEff #-}

mergeEffH ::
    forall fr' e r efs fr u c.
    ( Freer c fr'
    , Freer c fr
    , Union u
    , c (Eff u fr (e ': r) efs)
    , HFunctor (u r)
    , HFunctor e
    ) =>
    Hefty fr' (e :+: LiftIns (Eff u fr r efs)) ~> Eff u fr (e ': r) efs
mergeEffH =
    interpretFreer
        ( caseH
            (send0H . hfmap mergeEffH)
            (raiseH . unliftIns)
        )
        . unHefty

send0 :: (Freer c fr, Union u, HeadIns e) => UnliftIfSingle e ~> Eff u fr eh (e ': r)
send0 = Hefty . liftIns . EffUnion . R1 . inject0 . liftInsIfSingle
{-# INLINE send0 #-}

send1 :: (Freer c fr, Union u, HeadIns e1) => UnliftIfSingle e1 ~> Eff u fr eh (e2 ': e1 ': r)
send1 = Hefty . liftIns . EffUnion . R1 . weaken . inject0 . liftInsIfSingle
{-# INLINE send1 #-}

send0H :: (Freer c fr, Union u) => e (Eff u fr (e ': r) ef) ~> Eff u fr (e ': r) ef
send0H = Hefty . liftIns . EffUnion . L1 . inject0
{-# INLINE send0H #-}

send1H :: (Freer c fr, Union u) => e1 (Eff u fr (e2 ': e1 ': r) ef) ~> Eff u fr (e2 ': e1 ': r) ef
send1H = Hefty . liftIns . EffUnion . L1 . weaken . inject0
{-# INLINE send1H #-}

runEff :: forall f fr u c. (Freer c fr, Union u, c f) => Eff u fr '[] '[LiftIns f] ~> f
runEff = interpretAll $ id |+ exhaust
{-# INLINE runEff #-}

runPure :: forall a fr u c. (Freer c fr, Union u, c Identity) => Eff u fr '[] '[] a -> a
runPure = runIdentity . interpretAll exhaust
{-# INLINE runPure #-}

untagEff ::
    forall tag e r ehs fr u c.
    (Freer c fr, Union u, HFunctor (u ehs)) =>
    Eff u fr ehs (LiftIns (e # tag) ': r) ~> Eff u fr ehs (LiftIns e ': r)
untagEff = transform unTag
{-# INLINE untagEff #-}

untagEffH ::
    forall tag e r efs fr u c.
    (Freer c fr, Union u, HFunctor (u (e ## tag ': r))) =>
    Eff u fr (e ## tag ': r) efs ~> Eff u fr (e ': r) efs
untagEffH = transformH unTagH
{-# INLINE untagEffH #-}

-- keyed effects

instance
    (MemberRec u (LiftIns (key #> e)) efs, LiftIns (key #> e) ~ FromJust (Lookup key efs)) =>
    InjectInsBy key e (EffUnion u ehs efs f)
    where
    injectInsBy = EffUnion . R1 . injectRec . LiftIns . Key @key
    {-# INLINE injectInsBy #-}

instance
    (MemberRec u (key ##> e) ehs, key ##> e ~ FromJust (Lookup key ehs)) =>
    InjectSigBy key e (EffUnion u ehs efs)
    where
    injectSigBy = EffUnion . L1 . injectRec . KeyH @key
    {-# INLINE injectSigBy #-}

unkeyEff ::
    forall key e r ehs fr u c.
    (Freer c fr, Union u, HFunctor (u ehs)) =>
    Eff u fr ehs (LiftIns (key #> e) ': r) ~> Eff u fr ehs (LiftIns e ': r)
unkeyEff = transform unKey
{-# INLINE unkeyEff #-}

unkeyEffH ::
    forall key e r efs fr u c.
    (Freer c fr, Union u, HFunctor (u (key ##> e ': r))) =>
    Eff u fr (key ##> e ': r) efs ~> Eff u fr (e ': r) efs
unkeyEffH = transformH unKeyH
{-# INLINE unkeyEffH #-}

keySubsume ::
    forall key e r ehs fr u c.
    (Freer c fr, Union u, HFunctor (u ehs), MemberBy u key e r) =>
    Eff u fr ehs (LiftIns e ': r) ~> Eff u fr ehs r
keySubsume = interpretRec $ sendInsBy @key
{-# INLINE keySubsume #-}

keySubsumeH ::
    forall key e r efs fr u c.
    (Freer c fr, HFunctorUnion u, HFunctor e, ForallHFunctor u r, MemberHBy u key e r) =>
    Eff u fr (e ': r) efs ~> Eff u fr r efs
keySubsumeH = interpretRecH $ sendSigBy @key
{-# INLINE keySubsumeH #-}

end :: Union u => u '[] f a -> x
end = exhaust
{-# INLINE end #-}
