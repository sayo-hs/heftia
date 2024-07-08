{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Hefty where

import Control.Applicative (Alternative)
import Control.Effect (SendIns (..), SendSig (..), type (~>))
import Control.Effect.Key (ByKey (ByKey), SendInsBy, SendSigBy, key, sendInsBy, sendSigBy)
import Control.Freer (Freer (liftIns), InjectIns, InjectInsBy, StateKey, injectIns, injectInsBy)
import Control.Monad (MonadPlus)
import Control.Monad.Base (MonadBase)
import Control.Monad.Fix (MonadFix, mfix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.RWS.Class (MonadRWS)
import Control.Monad.Reader.Class (MonadReader, ask, local)
import Control.Monad.State.Class (MonadState, get, put)
import Control.Monad.Writer.Class (MonadWriter, listen, pass, tell)
import Data.Effect (InsClass, SigClass)
import Data.Effect.Fail (Fail)
import Data.Effect.Fail qualified as E
import Data.Effect.Fix (Fix)
import Data.Effect.Fix qualified as E
import Data.Effect.Reader (Ask, Local, ask'', local'')
import Data.Effect.State (State, get'', put'')
import Data.Effect.Unlift (UnliftIO, pattern WithRunInIO)
import Data.Effect.Writer (Tell, WriterH, listen'', tell'')
import Data.Function ((&))
import Data.Kind (Type)
import Data.Tuple (swap)
import UnliftIO (MonadUnliftIO, withRunInIO)

newtype
    Hefty
        (f :: InsClass -> Type -> Type)
        (e :: SigClass)
        (a :: Type) = Hefty
    {unHefty :: f (e (Hefty f e)) a}

deriving newtype instance Functor (f (e (Hefty f e))) => Functor (Hefty f e)
deriving newtype instance Applicative (f (e (Hefty f e))) => Applicative (Hefty f e)
deriving newtype instance Alternative (f (e (Hefty f e))) => Alternative (Hefty f e)
deriving newtype instance Monad (f (e (Hefty f e))) => Monad (Hefty f e)
deriving newtype instance MonadPlus (f (e (Hefty f e))) => MonadPlus (Hefty f e)
deriving newtype instance (MonadBase b (f (e (Hefty f e))), Monad b) => MonadBase b (Hefty f e)

deriving newtype instance Foldable (f (e (Hefty f e))) => Foldable (Hefty f e)
deriving stock instance Traversable (f (e (Hefty f e))) => Traversable (Hefty f e)
deriving newtype instance Eq (f (e (Hefty f e)) a) => Eq (Hefty f e a)
deriving newtype instance Ord (f (e (Hefty f e)) a) => Ord (Hefty f e a)
deriving newtype instance Read (f (e (Hefty f e)) a) => Read (Hefty f e a)
deriving newtype instance Show (f (e (Hefty f e)) a) => Show (Hefty f e a)

overHefty ::
    (f (e (Hefty f e)) a -> f' (e' (Hefty f' e')) b) ->
    Hefty f e a ->
    Hefty f' e' b
overHefty f = Hefty . f . unHefty
{-# INLINE overHefty #-}

instance (Freer c fr, InjectIns e (e' (Hefty fr e'))) => SendIns e (Hefty fr e') where
    sendIns = Hefty . liftIns . injectIns
    {-# INLINE sendIns #-}

instance (Freer c fr, InjectSig e e') => SendSig e (Hefty fr e') where
    sendSig = Hefty . liftIns . injectSig
    {-# INLINE sendSig #-}

class InjectSig e (e' :: SigClass) where
    injectSig :: e f ~> e' f

instance (Freer c fr, InjectInsBy key e (e' (Hefty fr e'))) => SendInsBy key e (Hefty fr e') where
    sendInsBy = Hefty . liftIns . injectInsBy @key
    {-# INLINE sendInsBy #-}

instance (Freer c fr, InjectSigBy key e e') => SendSigBy key e (Hefty fr e') where
    sendSigBy = Hefty . liftIns . injectSigBy @key
    {-# INLINE sendSigBy #-}

class InjectSigBy key e (e' :: SigClass) | key e' -> e where
    injectSigBy :: e f ~> e' f

instance
    ( Freer c fr
    , InjectInsBy ReaderKey (Ask r) (e (Hefty fr e))
    , InjectSigBy ReaderKey (Local r) e
    , Monad (fr (e (Hefty fr e)))
    ) =>
    MonadReader r (Hefty fr e)
    where
    ask = ask'' @ReaderKey
    local = local'' @ReaderKey
    {-# INLINE ask #-}
    {-# INLINE local #-}

data ReaderKey

instance
    ( Freer c fr
    , InjectInsBy WriterKey (Tell w) (e (Hefty fr e))
    , InjectSigBy WriterKey (WriterH w) e
    , Monoid w
    , Monad (fr (e (Hefty fr e)))
    ) =>
    MonadWriter w (Hefty fr e)
    where
    tell = tell'' @WriterKey
    listen = fmap swap . listen'' @WriterKey
    pass m = pass (ByKey m) & key @WriterKey
    {-# INLINE tell #-}
    {-# INLINE listen #-}

data WriterKey

instance
    (Freer c fr, InjectInsBy StateKey (State s) (e (Hefty fr e)), Monad (fr (e (Hefty fr e)))) =>
    MonadState s (Hefty fr e)
    where
    get = get'' @StateKey
    put = put'' @StateKey
    {-# INLINE get #-}
    {-# INLINE put #-}

instance
    ( Freer c fr
    , InjectInsBy ReaderKey (Ask r) (e (Hefty fr e))
    , InjectSigBy ReaderKey (Local r) e
    , InjectInsBy WriterKey (Tell w) (e (Hefty fr e))
    , InjectSigBy WriterKey (WriterH w) e
    , InjectInsBy StateKey (State s) (e (Hefty fr e))
    , Monoid w
    , Monad (fr (e (Hefty fr e)))
    ) =>
    MonadRWS r w s (Hefty fr e)

instance (Freer c fr, InjectIns IO (e (Hefty fr e)), Monad (fr (e (Hefty fr e)))) => MonadIO (Hefty fr e) where
    liftIO = sendIns
    {-# INLINE liftIO #-}

instance (Freer c fr, InjectIns Fail (e (Hefty fr e)), Monad (fr (e (Hefty fr e)))) => MonadFail (Hefty fr e) where
    fail = E.fail
    {-# INLINE fail #-}

instance (Freer c fr, InjectSig Fix e, Monad (fr (e (Hefty fr e)))) => MonadFix (Hefty fr e) where
    mfix = E.mfix
    {-# INLINE mfix #-}

instance
    ( Freer c fr
    , InjectIns IO (e (Hefty fr e))
    , InjectSig UnliftIO e
    , Monad (fr (e (Hefty fr e)))
    ) =>
    MonadUnliftIO (Hefty fr e)
    where
    withRunInIO f = Hefty . liftIns . injectSig $ WithRunInIO f
    {-# INLINE withRunInIO #-}
