-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Data.Hefty.Sum where

import Control.Effect.Class (type (~>))
import Control.Effect.Class.Machinery.HFunctor (HFunctor, caseH, hfmap, type (:+:))
import Control.Freer (Freer, interpretFreer, liftIns)
import Control.Hefty (Hefty, overHefty, unHefty)

interpretRecRWith ::
    forall r l f c.
    Freer c f =>
    ((Hefty f (l :+: r) ~> Hefty f l) -> l (Hefty f (l :+: r)) ~> l (Hefty f l)) ->
    ((Hefty f (l :+: r) ~> Hefty f l) -> r (Hefty f (l :+: r)) ~> Hefty f l) ->
    Hefty f (l :+: r) ~> Hefty f l
interpretRecRWith f i =
    overHefty $
        interpretFreer $
            caseH
                (liftIns . f int)
                (unHefty . i int)
  where
    int :: Hefty f (l :+: r) ~> Hefty f l
    int = interpretRecRWith f i
    {-# INLINE int #-}

interpretRecR ::
    forall r l f c.
    (Freer c f, HFunctor l, HFunctor r) =>
    (r (Hefty f l) ~> Hefty f l) ->
    Hefty f (l :+: r) ~> Hefty f l
interpretRecR i =
    overHefty $
        interpretFreer $
            caseH
                (liftIns . hfmapInt)
                (unHefty . i . hfmapInt)
  where
    hfmapInt :: HFunctor e => e (Hefty f (l :+: r)) ~> e (Hefty f l)
    hfmapInt = hfmap $ interpretRecR i
    {-# INLINE hfmapInt #-}
