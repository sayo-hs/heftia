module Control.Monad.Trans.Heftia where

import Control.Heftia.Trans (TransHeftia, hoistHeftia, interpretT, liftLower)
import Control.Hefty (HFunctor, Signature)
import Control.Monad.Cont (ContT)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Natural (type (~>))
import Data.Coerce (Coercible, coerce)
import Data.Kind (Type)

class TransHeftia Monad h => MonadTransHeftia h where
    interpretK ::
        (Monad m, HFunctor sig) =>
        (sig (ContT b m) ~> ContT b m) ->
        h m sig a ->
        ContT b m a
    interpretK = interpretTT
    {-# INLINE interpretK #-}

    reinterpretK ::
        (Monad m, HFunctor sig) =>
        (sig (ContT b (h m sig)) ~> ContT b (h m sig)) ->
        h m sig a ->
        ContT b (h m sig) a
    reinterpretK = reinterpretTT
    {-# INLINE reinterpretK #-}

    interpretTT ::
        (Monad m, MonadTrans t, Monad (t m), HFunctor sig) =>
        (sig (t m) ~> t m) ->
        h m sig a ->
        t m a
    interpretTT = interpretT lift
    {-# INLINE interpretTT #-}

    reinterpretTT ::
        forall m t n sig a.
        (Monad m, MonadTrans t, Coercible n (h m sig), Monad (t n), Monad n, HFunctor sig) =>
        (sig (t n) ~> t n) ->
        h m sig a ->
        t n a
    reinterpretTT f = interpretTT f . hoistHeftia (coerce . liftLower @Monad @h @sig)
    {-# INLINE reinterpretTT #-}

reinterpretTTViaFinal ::
    forall h m t n sig a.
    ( MonadTransHeftia h
    , Monad m
    , MonadTrans t
    , Coercible n (h m sig)
    , Monad (t n)
    , Monad n
    , HFunctor sig
    ) =>
    (sig (t n) ~> t n) ->
    h m sig a ->
    t n a
reinterpretTTViaFinal = interpretT $ lift . coerce . liftLower @Monad @h @sig
{-# INLINE reinterpretTTViaFinal #-}

newtype HeftiaT (h :: (Type -> Type) -> Signature -> Type -> Type) sig m a = HeftiaT
    {runHeftiaT :: h m sig a}
    deriving newtype (Functor, Applicative, Monad)
    deriving stock (Foldable, Traversable)

instance (MonadTransHeftia h, HFunctor sig) => MonadTrans (HeftiaT h sig) where
    lift = HeftiaT . liftLower
