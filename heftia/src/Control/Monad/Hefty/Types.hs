-- SPDX-License-Identifier: MPL-2.0
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Hefty.Types where

import Control.Applicative (Alternative, empty, (<|>))
import Control.Effect (SendIns, SendSig, sendIns, sendSig, type (~>))
import Control.Effect.Key (ByKey (ByKey), SendInsBy, SendSigBy, key, sendInsBy, sendSigBy)
import Control.Monad (MonadPlus)
import Control.Monad.Error.Class (MonadError, catchError, throwError)
import Control.Monad.Fix (MonadFix, mfix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.RWS (MonadRWS)
import Control.Monad.Reader.Class (MonadReader, ask, local)
import Control.Monad.State.Class (MonadState, get, put)
import Control.Monad.Writer.Class (MonadWriter, listen, pass, tell)
import Data.Effect.Except
import Data.Effect.Fail (Fail)
import Data.Effect.Fail qualified as E
import Data.Effect.Fix (Fix)
import Data.Effect.Fix qualified as E
import Data.Effect.NonDet (ChooseH, Empty, chooseH)
import Data.Effect.NonDet qualified as E
import Data.Effect.OpenUnion.Internal.FO (Lookup, MemberBy, Union, inj, type (<|))
import Data.Effect.OpenUnion.Internal.HO (LookupH, MemberHBy, UnionH, injH, type (<<|))
import Data.Effect.Reader (Ask, Local, ask'', local'')
import Data.Effect.State (State, get'', put'')
import Data.Effect.Unlift (UnliftIO)
import Data.Effect.Unlift qualified as E
import Data.Effect.Writer (Tell, WriterH, listen'', tell'')
import Data.FTCQueue (FTCQueue, tsingleton, (|>))
import Data.Function ((&))
import Data.Kind (Type)
import Data.Tuple (swap)
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

instance (e <| ef) => SendIns e (Eff eh ef) where
    sendIns = send
    {-# INLINE sendIns #-}

instance (e <<| eh) => SendSig e (Eff eh ef) where
    sendSig = sendH
    {-# INLINE sendSig #-}

instance (MemberBy key ef, e ~ Lookup key ef) => SendInsBy key e (Eff eh ef) where
    sendInsBy = send
    {-# INLINE sendInsBy #-}

instance (MemberHBy key eh, e ~ LookupH key eh) => SendSigBy key e (Eff eh ef) where
    sendSigBy = sendH
    {-# INLINE sendSigBy #-}

instance
    ( SendInsBy ReaderKey (Ask r) (Eff eh ef)
    , SendSigBy ReaderKey (Local r) (Eff eh ef)
    )
    => MonadReader r (Eff eh ef)
    where
    ask = ask'' @ReaderKey
    local = local'' @ReaderKey
    {-# INLINE ask #-}
    {-# INLINE local #-}

data ReaderKey

instance
    ( SendInsBy WriterKey (Tell w) (Eff eh ef)
    , SendSigBy WriterKey (WriterH w) (Eff eh ef)
    , Monoid w
    )
    => MonadWriter w (Eff eh ef)
    where
    tell = tell'' @WriterKey
    listen = fmap swap . listen'' @WriterKey
    pass m = pass (ByKey m) & key @WriterKey
    {-# INLINE tell #-}
    {-# INLINE listen #-}

data WriterKey

instance
    (SendInsBy StateKey (State s) (Eff eh ef))
    => MonadState s (Eff eh ef)
    where
    get = get'' @StateKey
    put = put'' @StateKey
    {-# INLINE get #-}
    {-# INLINE put #-}

data StateKey

instance
    ( SendInsBy ErrorKey (Throw e) (Eff eh ef)
    , SendSigBy ErrorKey (Catch e) (Eff eh ef)
    )
    => MonadError e (Eff eh ef)
    where
    throwError = throw'' @ErrorKey
    catchError = catch'' @ErrorKey
    {-# INLINE throwError #-}
    {-# INLINE catchError #-}

data ErrorKey

instance
    ( SendInsBy ReaderKey (Ask r) (Eff eh ef)
    , SendSigBy ReaderKey (Local r) (Eff eh ef)
    , SendInsBy WriterKey (Tell w) (Eff eh ef)
    , SendSigBy WriterKey (WriterH w) (Eff eh ef)
    , SendInsBy StateKey (State s) (Eff eh ef)
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

infixr 3 $
infixr 4 $$

-- | Type-level infix applcation for functors.
type (f :: Type -> Type) $ a = f a

-- | Type-level infix applcation for higher-order functors.
type (h :: (Type -> Type) -> Type -> Type) $$ f = h f

{- | Type synonym for an interpreter function.
@Interpreter e m ans@ transforms an effect @e@ into a computation in @m@ where the result has the type (answer type) @ans@.
-}
type Interpreter e m (ans :: Type) = forall x. e x -> (x -> m ans) -> m ans

{- | Type alias for an elaborator function.
An 'Elaborator' is an interpreter for higher-order effects.
-}
type Elaborator e m ans = Interpreter (e m) m ans

{- | Type alias for an elaborator transformation.
An 'Elab' transforms an higher-order effect @e f@ into @f@.
-}
type Elab e f = e f ~> f

sendUnion :: Union ef a -> Eff eh ef a
sendUnion = sendUnionBy pure
{-# INLINE sendUnion #-}

sendUnionBy :: (a -> Eff eh ef ans) -> Union ef a -> Eff eh ef ans
sendUnionBy k u = Op (Right u) (tsingleton k)
{-# INLINE sendUnionBy #-}

sendUnionH :: UnionH eh (Eff eh ef) a -> Eff eh ef a
sendUnionH = sendUnionHBy pure
{-# INLINE sendUnionH #-}

sendUnionHBy :: (a -> Eff eh ef ans) -> UnionH eh (Eff eh ef) a -> Eff eh ef ans
sendUnionHBy k u = Op (Left u) (tsingleton k)
{-# INLINE sendUnionHBy #-}

send :: (e <| ef) => e ~> Eff eh ef
send = sendUnion . inj
{-# INLINE send #-}

sendH :: (e <<| eh) => e (Eff eh ef) ~> Eff eh ef
sendH = sendUnionH . injH
{-# INLINE sendH #-}

send0 :: e ~> Eff eh (e ': ef)
send0 = send
{-# INLINE send0 #-}

send0H :: e (Eff (e ': eh) ef) ~> Eff (e ': eh) ef
send0H = sendH
{-# INLINE send0H #-}
