module Data.Free.Union where

import Control.Natural (type (~>))

class s <:: t where
    weakenIns :: s ~> t
