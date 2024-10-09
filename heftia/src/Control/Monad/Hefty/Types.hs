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
import Control.Effect (SendFOE, SendHOE, sendFOE, sendHOE, type (~>))
import Control.Effect.Key (ByKey (ByKey), SendFOEBy, SendHOEBy, key, sendFOEBy, sendHOEBy)
import Control.Monad (MonadPlus)
import Control.Monad.Error.Class (MonadError, catchError, throwError)
import Control.Monad.Fix (MonadFix, mfix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.RWS (MonadRWS)
import Control.Monad.Reader.Class (MonadReader, ask, local)
import Control.Monad.State.Class (MonadState, get, put)
import Control.Monad.Writer.Class (MonadWriter, listen, pass, tell)
import Data.Effect.Except (Catch, Throw, catch'', throw'')
import Data.Effect.Fail (Fail)
import Data.Effect.Fail qualified as E
import Data.Effect.Fix (Fix)
import Data.Effect.Fix qualified as E
import Data.Effect.Key (Key (Key), KeyH (KeyH))
import Data.Effect.NonDet (ChooseH, Empty, chooseH)
import Data.Effect.NonDet qualified as E
import Data.Effect.OpenUnion.Internal (ElemAt)
import Data.Effect.OpenUnion.Internal.FO (MemberBy, Union, inj, inj0, injN, type (<|))
import Data.Effect.OpenUnion.Internal.HO (MemberHBy, UnionH, inj0H, injH, injNH, type (<<|))
import Data.Effect.OpenUnion.Sum (SumToRecUnionList)
import Data.Effect.Reader (Ask, Local, ask'', local'')
import Data.Effect.State (State, get'', put'')
import Data.Effect.Unlift (UnliftIO)
import Data.Effect.Unlift qualified as E
import Data.Effect.Writer (Tell, WriterH, listen'', tell'')
import Data.FTCQueue (FTCQueue, tsingleton, (|>))
import Data.Function ((&))
import Data.Kind (Type)
import Data.Tuple (swap)
import GHC.TypeNats (KnownNat)
import UnliftIO (MonadUnliftIO, withRunInIO)

{- | The 'Eff' monad represents computations with effects.
It supports higher-order effects @eh@ and first-order effects @ef@.
-}
data Eff eh ef a
    = -- | A pure value.
      Val a
    | -- | An effectful operation, which can be either a higher-order effect or a first-order effect.
      forall x. Op
        (Either (UnionH eh (Eff eh ef) x) (Union ef x))
        (FTCQueue (Eff eh ef) x a)
        -- ^ the continuation of the operation.

infixr 4 :!!

{- | Type-level infix operator for 'Eff'.
Allows writing @eh :!! ef@ instead of @Eff eh ef@.
-}
type (:!!) = Eff

infixr 5 !!

{- | An infix operator version of t`Eff` for sum notation.

Example:

@Span t'Control.Monad.Hefty.Types.!!' FileSystem t'Data.Effect.OpenUnion.Sum.+' Time t'Data.Effect.OpenUnion.Sum.+' Log t'Data.Effect.OpenUnion.Sum.+' t'IO' t'Control.Effect.~>' t'IO'@
-}
type eh !! ef = SumToRecUnionList UnionH eh :!! SumToRecUnionList Union ef

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

-- | Send a first-order effect @e@ to the t`Eff` carrier.
send :: (e <| ef) => e ~> Eff eh ef
send = sendUnion . inj
{-# INLINE send #-}

-- | Send a higher-order effect @e@ to the t`Eff` carrier.
sendH :: (e <<| eh) => e (Eff eh ef) ~> Eff eh ef
sendH = sendUnionH . injH
{-# INLINE sendH #-}

-- | Send the first-order effect @e@ at the head of the list to the t`Eff` carrier.
send0 :: e ~> Eff eh (e ': ef)
send0 = sendUnion . inj0
{-# INLINE send0 #-}

-- | Send the higher-order effect @e@ at the head of the list to the t`Eff` carrier.
send0H :: e (Eff (e ': eh) ef) ~> Eff (e ': eh) ef
send0H = sendUnionH . inj0H
{-# INLINE send0H #-}

-- | Send the @i@-th first-order effect in the list to the t`Eff` carrier.
sendN :: forall i ef eh. (KnownNat i) => ElemAt i ef ~> Eff eh ef
sendN = sendUnion . injN @i
{-# INLINE sendN #-}

-- | Send the @i@-th higher-order effect in the list to the t`Eff` carrier.
sendNH :: forall i eh ef. (KnownNat i) => ElemAt i eh (Eff eh ef) ~> Eff eh ef
sendNH = sendUnionH . injNH @i
{-# INLINE sendNH #-}

-- | Send an open union of all first-order effects to the t`Eff` carrier.
sendUnion :: Union ef a -> Eff eh ef a
sendUnion = sendUnionBy pure
{-# INLINE sendUnion #-}

-- | Send an open union of all first-order effects, along with its continuation, to the t`Eff` carrier.
sendUnionBy :: (a -> Eff eh ef ans) -> Union ef a -> Eff eh ef ans
sendUnionBy k u = Op (Right u) (tsingleton k)
{-# INLINE sendUnionBy #-}

-- | Send an open union of all higher-order effects to the t`Eff` carrier.
sendUnionH :: UnionH eh (Eff eh ef) a -> Eff eh ef a
sendUnionH = sendUnionHBy pure
{-# INLINE sendUnionH #-}

-- | Send an open union of all higher-order effects, along with its continuation, to the t`Eff` carrier.
sendUnionHBy :: (a -> Eff eh ef ans) -> UnionH eh (Eff eh ef) a -> Eff eh ef ans
sendUnionHBy k u = Op (Left u) (tsingleton k)
{-# INLINE sendUnionHBy #-}

instance Functor (Eff eh ef) where
    fmap f = \case
        Val x -> Val (f x)
        Op u q -> Op u (q |> (Val . f))
    {-# INLINE fmap #-}

instance Applicative (Eff eh ef) where
    pure = Val
    {-# INLINE pure #-}

    Val f <*> Val x = Val $ f x
    Val f <*> Op u q = Op u (q |> (Val . f))
    Op u q <*> m = Op u (q |> (<$> m))
    {-# INLINE (<*>) #-}

instance Monad (Eff eh ef) where
    m >>= k = case m of
        Val x -> k x
        Op e q -> Op e (q |> k)
    {-# INLINE (>>=) #-}

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
