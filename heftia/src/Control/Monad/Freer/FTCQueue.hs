-- Come from: https://hackage.haskell.org/package/freer-simple-1.2.1.2/docs/Control-Monad-Freer.html#t:Eff
-- BSD3, (c) 2016 Allele Dev; 2017 Ixperta Solutions s.r.o.; 2017 Alexis King

module Control.Monad.Freer.FTCQueue where

import Control.Freer
import Control.Monad.Cont (ContT (ContT))
import Control.Monad.Freer
import Control.Monad.Identity (Identity (Identity))
import Data.FTCQueue (FTCQueue, ViewL (TOne, (:|)), tsingleton, tviewl, (><), (|>))

data FreerFTCQueue f a
    = Val a
    | forall b. E (f b) (Arrs f b a)

type Arrs f = FTCQueue (FreerFTCQueue f)
type Arr f a b = a -> FreerFTCQueue f b

instance Functor (FreerFTCQueue f) where
    fmap f (Val x) = Val (f x)
    fmap f (E u q) = E u (q |> (Val . f))
    {-# INLINE fmap #-}

instance Applicative (FreerFTCQueue f) where
    pure = Val
    {-# INLINE pure #-}

    Val f <*> Val x = Val $ f x
    Val f <*> E u q = E u (q |> (Val . f))
    E u q <*> m = E u (q |> (`fmap` m))
    {-# INLINE (<*>) #-}

instance Monad (FreerFTCQueue f) where
    Val x >>= k = k x
    E u q >>= k = E u (q |> k)
    {-# INLINE (>>=) #-}

{- | “Sends” an effect, which should be a value defined as part of an effect
 algebra (see the module documentation for "Control.Monad.Freer"), to an
 effectful computation. This is used to connect the definition of an effect to
 the 'Eff' monad so that it can be used and handled.
-}
liftInsFQ :: f a -> FreerFTCQueue f a
liftInsFQ t = E t (tsingleton Val)
{-# INLINE liftInsFQ #-}

{- | Function application in the context of an array of effects,
 @'Arrs' effs b w@.
-}
qApp :: Arrs f b w -> b -> FreerFTCQueue f w
qApp q' x = case tviewl q' of
    TOne k -> k x
    k :| t -> case k x of
        Val y -> qApp t y
        E u q -> E u (q >< t)

{- | Composition of effectful arrows ('Arrs'). Allows for the caller to change
 the effect environment, as well.
-}
qComp :: Arrs f a b -> (FreerFTCQueue f b -> FreerFTCQueue f' c) -> Arr f' a c
qComp g h a = h $ qApp g a

-- | Given a request, either handle it or relay it.
handleRelay ::
    -- | Handle a pure value.
    (a -> FreerFTCQueue g b) ->
    -- | Handle a request for effect of type @eff :: * -> *@.
    (forall v. f v -> Arr g v b -> FreerFTCQueue g b) ->
    FreerFTCQueue f a ->
    -- | Result with effects of type @eff :: * -> *@ handled.
    FreerFTCQueue g b
handleRelay ret h = loop
  where
    loop (Val x) = ret x
    loop (E e q) = h e k
      where
        k = qComp q loop
{-# INLINE handleRelay #-}

instance Freer Monad FreerFTCQueue where
    liftIns t = E t (tsingleton Val)
    interpretFreer h = loop
      where
        loop (Val x) = pure x
        loop (E e q) = h e >>= loop . qApp q
    {-# INLINE liftIns #-}
    {-# INLINE interpretFreer #-}

instance MonadFreer Monad FreerFTCQueue where
    interpretFreerK f = loop
      where
        loop (Val x) = pure x
        loop (E e q) = f e >>= loop . qApp q
    {-# INLINE interpretFreerK #-}
