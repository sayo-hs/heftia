-- SPDX-License-Identifier: MPL-2.0

module Control.Monad.Hefty.Types where

import Control.Effect (type (~>))
import Data.Effect.OpenUnion.Internal.FO (Union, inj, type (<!))
import Data.Effect.OpenUnion.Internal.HO (UnionH, injH, type (<!!))
import Data.FTCQueue (FTCQueue, tsingleton, (|>))
import Data.Kind (Type)

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

send :: (e <! ef) => e ~> Eff eh ef
send = sendUnion . inj
{-# INLINE send #-}

sendH :: (e <!! eh) => e (Eff eh ef) ~> Eff eh ef
sendH = sendUnionH . injH
{-# INLINE sendH #-}

send0 :: e ~> Eff eh (e ': ef)
send0 = send
{-# INLINE send0 #-}

send0H :: e (Eff (e ': eh) ef) ~> Eff (e ': eh) ef
send0H = sendH
{-# INLINE send0H #-}
