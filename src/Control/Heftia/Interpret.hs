{-# LANGUAGE UndecidableInstances #-}

module Control.Heftia.Interpret where

import Control.Applicative (Alternative)
import Control.Heftia (Heftia, interpretH, liftSig, reinterpretH, translateH)
import Control.Hefty (HFunctor, Signature, hmap)
import Control.Monad (MonadPlus)
import Control.Natural (type (~>))
import Data.Constraint (Constraint)
import Data.Hefty.Union (Member, Union, decomp, inject, project, weakenL, weakenR)
import Data.Kind (Type)

class Interpret i where
    {-# MINIMAL send, interpret, reinterpret, translate, interpose, intercept, raise #-}

    type Anycast i (e :: Signature) (es :: [Signature]) :: Constraint
    type Surface i (e :: Signature) :: Constraint
    type Bulk i (es :: [Signature]) :: Constraint

    send :: (Anycast i e es, Surface i e, Bulk i es) => e (i es) a -> i es a

    interpret ::
        (Surface i e, Bulk i es, Bulk i (e ': es)) =>
        (e (i es) ~> i es) ->
        i (e ': es) a ->
        i es a

    reinterpret ::
        (Surface i e, Bulk i (e ': es)) =>
        (e (i (e ': es)) ~> i (e ': es)) ->
        i (e ': es) a ->
        i (e ': es) a

    translate ::
        (Surface i e, Surface i e', Bulk i (e ': es), Bulk i (e' ': es)) =>
        (e (i (e' ': es)) ~> e' (i (e' ': es))) ->
        i (e ': es) a ->
        i (e' ': es) a

    interpose ::
        (Anycast i e es, Surface i e, Bulk i es) =>
        (e (i es) ~> i es) ->
        i es a ->
        i es a

    intercept ::
        (Anycast i e es, Surface i e, Bulk i es) =>
        (e (i es) ~> e (i es)) ->
        i es a ->
        i es a

    raise :: forall e es a. (Bulk i es, Bulk i (e ': es)) => i es a -> i (e ': es) a

    subsume ::
        (Anycast i e es, Surface i e, Bulk i es, Bulk i (e ': es)) =>
        i (e ': es) a ->
        i es a
    subsume = interpret send
    {-# INLINE subsume #-}

newtype ViaHeftiaUnion h (u :: [Signature] -> Signature) es (a :: Type) = ViaHeftiaUnion
    {runViaHeftiaUnion :: h (u es) a}
    deriving newtype (Functor, Applicative, Alternative, Monad, MonadPlus)

instance (Heftia c h, Union u) => Interpret (ViaHeftiaUnion h u) where
    type Anycast (ViaHeftiaUnion _ u) e es = Member u e es
    type Surface _ e = HFunctor e
    type Bulk (ViaHeftiaUnion _ u) es = HFunctor (u es)

    send = ViaHeftiaUnion . liftSig . inject . hmap runViaHeftiaUnion

    interpret i (ViaHeftiaUnion m) =
        ViaHeftiaUnion $ (`interpretH` m) \u ->
            case decomp u of
                Left e -> runViaHeftiaUnion $ i $ hmap ViaHeftiaUnion e
                Right e -> liftSig e

    reinterpret i (ViaHeftiaUnion m) =
        ViaHeftiaUnion $ (`reinterpretH` m) \u ->
            case decomp u of
                Left e -> runViaHeftiaUnion $ i $ hmap ViaHeftiaUnion e
                Right e -> liftSig $ weakenR e

    translate f (ViaHeftiaUnion m) =
        ViaHeftiaUnion $ (`translateH` m) \u ->
            case decomp u of
                Left e -> weakenL $ hmap runViaHeftiaUnion $ f $ hmap ViaHeftiaUnion e
                Right e -> weakenR e

    interpose (f :: e (ViaHeftiaUnion h u es) ~> ViaHeftiaUnion h u es) (ViaHeftiaUnion m) =
        ViaHeftiaUnion $ (`reinterpretH` m) \u ->
            case project @_ @e u of
                Just e -> runViaHeftiaUnion . f $ hmap ViaHeftiaUnion e
                Nothing -> liftSig u

    intercept (f :: e (ViaHeftiaUnion h u es) ~> e (ViaHeftiaUnion h u es)) (ViaHeftiaUnion m) =
        ViaHeftiaUnion $ (`translateH` m) \u ->
            case project @_ @e u of
                Just e -> inject $ hmap runViaHeftiaUnion $ f $ hmap ViaHeftiaUnion e
                Nothing -> u

    raise = ViaHeftiaUnion . translateH weakenR . runViaHeftiaUnion
