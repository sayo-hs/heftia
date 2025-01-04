{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2024 Sayo Koyoneda
License     :  MPL-2.0 (see the LICENSE file)
Maintainer  :  ymdfield@outlook.jp

This module defines the t'Eff' monad and related fundamental types and functions.
Please refer to the documentation of the [top-level module]("Control.Monad.Hefty").
-}
module Control.Monad.Hefty.Types where

import Control.Applicative (Alternative, empty, (<|>))
import Control.Effect (Free, unEff, type (~>))
import Control.Effect qualified as D
import Control.Monad (MonadPlus)
import Control.Monad.Error.Class (MonadError, catchError, throwError)
import Control.Monad.Fix (MonadFix, mfix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.RWS (MonadRWS)
import Control.Monad.Reader.Class (MonadReader, ask, local)
import Control.Monad.State.Class (MonadState, get, put)
import Control.Monad.Writer.Class (MonadWriter, listen, pass, tell)
import Data.Coerce (coerce)
import Data.Effect.Except (Catch, Throw, catch'', throw'')
import Data.Effect.NonDet (ChooseH, Empty, chooseH)
import Data.Effect.NonDet qualified as E
import Data.Effect.OpenUnion
import Data.Effect.Reader (Ask, Local, ask'', local'')
import Data.Effect.State (State, get'', put'')
import Data.Effect.Writer (Tell, WriterH, listen'', tell'')
import Data.FTCQueue (FTCQueue, ViewL (..), tsingleton, tviewl, (><), (|>))
import Data.Function ((&))
import Data.Kind (Type)
import Data.Tuple (swap)
import GHC.TypeNats (KnownNat)
import UnliftIO (MonadUnliftIO, withRunInIO)

{-
import Data.Effect.Fail (Fail)
import Data.Effect.Fail qualified as E
import Data.Effect.Fix (Fix)
import Data.Effect.Fix qualified as E
import Data.Effect.Unlift (UnliftIO)
import Data.Effect.Unlift qualified as E
import Data.Effect.OpenUnion.Sum (SumToRecUnionList)
-}

data Freer f a
    = -- | A pure value.
      Val a
    | -- | An effectful operation, which can be either a higher-order effect or a first-order effect.
      forall x. Op
        (f x)
        (FTCQueue (Freer f) x a)
        -- ^ the continuation of the operation.

type Eff = D.Eff Freer

infixr 3 $
infixr 4 $$

-- | Type-level infix applcation for functors.
type (f :: Type -> Type) $ a = f a

-- | Type-level infix applcation for higher-order functors.
type (h :: (Type -> Type) -> Type -> Type) $$ f = h f

{- | Type alias for an interpreter function.

@Interpreter e m ans@ transforms an effect @e@ into a computation in @m@ where the result type (answer type) is @ans@.
-}
type Interpreter e m (ans :: Type) = forall x. e x -> (x -> m ans) -> m ans

{- | Type alias for an elaborator function.

An 'Elaborator' is an interpreter for higher-order effects.
-}
type Elaborator e m ans = Interpreter (e m) m ans

infix 2 ~~>

-- | Type alias for a natural transformation style elaborator.
type e ~~> f = e f ~> f

-- | Send an open union of all first-order effects, along with its continuation, to the t`Eff` carrier.
sendAnyBy :: (a -> Eff es ans) -> Union es (Eff es) a -> Eff es ans
sendAnyBy k u = D.Eff $ Op u (tsingleton $ unEff . k)
{-# INLINE sendAnyBy #-}

instance Functor (Freer f) where
    fmap f = \case
        Val x -> Val (f x)
        Op u q -> Op u (q |> (Val . f))
    {-# INLINE fmap #-}

instance Applicative (Freer f) where
    pure = Val
    {-# INLINE pure #-}

    Val f <*> Val x = Val $ f x
    Val f <*> Op u q = Op u (q |> (Val . f))
    Op u q <*> m = Op u (q |> (<$> m))
    {-# INLINE (<*>) #-}

instance Monad (Freer f) where
    m >>= k = case m of
        Val x -> k x
        Op e q -> Op e (q |> k)
    {-# INLINE (>>=) #-}

instance Free Monad Freer where
    liftFree f = Op f (tsingleton pure)
    runFree i = loop
      where
        loop = \case
            Val x -> pure x
            Op f q -> i f >>= k
              where
                k = loop . qApp q

    {-# INLINE liftFree #-}
    {-# INLINE runFree #-}

-- | Applies a value to a Kleisli arrow in 'FTCQueue' representation.
qApp :: FTCQueue (Freer f) a b -> a -> Freer f b
qApp q' x = case tviewl q' of
    TOne k -> k x
    k :| t -> case k x of
        Val y -> qApp t y
        Op u q -> Op u (q >< t)

{-

instance (e <| ef) => SendFOE e (Eff eh ef) where
    sendFOE = send
    {-# INLINE sendFOE #-}

instance (e <<| eh) => SendHOE e (Eff eh ef) where
    sendHOE = sendH
    {-# INLINE sendHOE #-}

instance (MemberBy key e ef) => SendFOEBy key e (Eff eh ef) where
    sendFOEBy = send . Key @key
    {-# INLINE sendFOEBy #-}

instance (MemberHBy key e eh) => SendHOEBy key e (Eff eh ef) where
    sendHOEBy = sendH . KeyH @key
    {-# INLINE sendHOEBy #-}

instance
    ( SendFOEBy ReaderKey (Ask r) (Eff eh ef)
    , SendHOEBy ReaderKey (Local r) (Eff eh ef)
    )
    => MonadReader r (Eff eh ef)
    where
    ask = ask'' @ReaderKey
    local = local'' @ReaderKey
    {-# INLINE ask #-}
    {-# INLINE local #-}

{- | A key to be attached to the effect targeted by the t`MonadReader` instance.

Since t`MonadReader` has a functional dependency on @r@, this is needed to uniquely specify @r@.
-}
data ReaderKey

instance
    ( SendFOEBy WriterKey (Tell w) (Eff eh ef)
    , SendHOEBy WriterKey (WriterH w) (Eff eh ef)
    , Monoid w
    )
    => MonadWriter w (Eff eh ef)
    where
    tell = tell'' @WriterKey
    listen = fmap swap . listen'' @WriterKey
    pass m = pass (ByKey m) & key @WriterKey
    {-# INLINE tell #-}
    {-# INLINE listen #-}

{- | A key to be attached to the effect targeted by the t'Control.Monad.Writer.Class.MonadWriter' instance.

Since t'Control.Monad.Writer.Class.MonadWriter' has a functional dependency on @w@, this is needed to uniquely specify @w@.
-}
data WriterKey

instance
    (SendFOEBy StateKey (State s) (Eff eh ef))
    => MonadState s (Eff eh ef)
    where
    get = get'' @StateKey
    put = put'' @StateKey
    {-# INLINE get #-}
    {-# INLINE put #-}

{- | A key to be attached to the effect targeted by the t`MonadState` instance.

Since t`MonadState` has a functional dependency on @s@, this is needed to uniquely specify @s@.
-}
data StateKey

instance
    ( SendFOEBy ErrorKey (Throw e) (Eff eh ef)
    , SendHOEBy ErrorKey (Catch e) (Eff eh ef)
    )
    => MonadError e (Eff eh ef)
    where
    throwError = throw'' @ErrorKey
    catchError = catch'' @ErrorKey
    {-# INLINE throwError #-}
    {-# INLINE catchError #-}

{- | A key to be attached to the effect targeted by the t`Control.Monad.Error.Class.MonadError` instance.

Since t`Control.Monad.Error.Class.MonadError` has a functional dependency on @e@, this is needed to uniquely specify @e@.
-}
data ErrorKey

instance
    ( SendFOEBy ReaderKey (Ask r) (Eff eh ef)
    , SendHOEBy ReaderKey (Local r) (Eff eh ef)
    , SendFOEBy WriterKey (Tell w) (Eff eh ef)
    , SendHOEBy WriterKey (WriterH w) (Eff eh ef)
    , SendFOEBy StateKey (State s) (Eff eh ef)
    , Monoid w
    )
    => MonadRWS r w s (Eff eh ef)

instance (Empty <| ef, ChooseH <<| eh) => Alternative (Eff eh ef) where
    empty = E.empty
    a <|> b = chooseH a b
    {-# INLINE empty #-}
    {-# INLINE (<|>) #-}

instance (Empty <| ef, ChooseH <<| eh) => MonadPlus (Eff eh ef)

instance (IO <| ef) => MonadIO (Eff eh ef) where
    liftIO = send
    {-# INLINE liftIO #-}

instance (Fail <| ef) => MonadFail (Eff eh ef) where
    fail = E.fail
    {-# INLINE fail #-}

instance (Fix <<| eh) => MonadFix (Eff eh ef) where
    mfix = E.mfix

instance (UnliftIO <<| eh, IO <| ef) => MonadUnliftIO (Eff eh ef) where
    withRunInIO = E.withRunInIO
    {-# INLINE withRunInIO #-}
-}
