-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2023-2025 Sayo Koyoneda
License     :  MPL-2.0 (see the LICENSE file)
Maintainer  :  ymdfield@outlook.jp

Effects that can be used to hold environmental values in the context.
Environmental values are immutable and do not change across procedures, but you
can modify the value within a local scope using the `local` operation.
-}
module Control.Monad.Hefty.Reader (module Data.Effect.Reader) where

import Data.Effect.Reader
