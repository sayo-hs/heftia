{-# LANGUAGE QuantifiedConstraints #-}

module Control.Monad.Trans.Heftier where

import Control.Hefty (HFunctor, Signature, hmap)
import Control.Hefty.Class (Heftier, liftSig)
import Control.Monad.Cont (ContT)
import Control.Monad.Identity (IdentityT (IdentityT), runIdentityT)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Natural (type (~>))
import Data.Coerce (Coercible, coerce)
import Data.Kind (Type)

class
    ( forall sig. HFunctor sig => MonadTrans (TransHeftier h sig)
    , forall m. Monad m => Heftier Monad (h m)
    ) =>
    MonadTransHeftier h
    where
    {-# MINIMAL hoistHeftier, interpretTT | interpretT #-}

    -- | Translate an underlying monad.
    hoistHeftier :: (Monad m, Monad n, HFunctor sig) => (m ~> n) -> h m sig a -> h n sig a
    hoistHeftier phi = interpretT (liftLower . phi) (liftSig @Monad)
    {-# INLINE hoistHeftier #-}

    interpretR :: (Monad m, HFunctor sig) => (sig m ~> m) -> h m sig a -> m a
    interpretR f = runIdentityT . interpretTT (IdentityT . f . hmap runIdentityT)
    {-# INLINE interpretR #-}

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
    reinterpretTT f = interpretTT f . hoistHeftier (coerce . liftLower @h @sig)
    {-# INLINE reinterpretTT #-}

    interpretT :: (Monad m, Monad n, HFunctor sig) => (m ~> n) -> (sig n ~> n) -> h m sig a -> n a
    interpretT phi i = interpretR i . hoistHeftier phi
    {-# INLINE interpretT #-}

interceptTViaFinal ::
    forall h m t n sig a.
    ( MonadTransHeftier h
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
interceptTViaFinal = interpretT $ lift . coerce . liftLower @h @sig
{-# INLINE interceptTViaFinal #-}

newtype TransHeftier (h :: (Type -> Type) -> Signature -> Type -> Type) sig m a = TransHeftier
    {getTransHeftier :: h m sig a}
    deriving newtype (Functor, Applicative, Monad)
    deriving stock (Foldable, Traversable)

liftLower :: (MonadTrans (TransHeftier h sig), Monad m) => m a -> h m sig a
liftLower = getTransHeftier . lift
