-- | Definitions using a type-level strength switch.

module Strongweak.Strength where

import Strongweak.Weaken ( type Weakened )
import Data.Kind ( type Type )

import Strongweak.Chain
import GHC.TypeNats ( type Natural )

-- | Strength enumeration: it's either strong, or weak.
--
-- Primarily interesting at the type level (using DataKinds).
data Strength = Strong | Weak

{- | Get either the strong or weak representation of a type, depending on the
     type-level "switch" provided.

This is intended to be used in data types that take a 'Strength' type. Define
your type using strong fields wrapped in @SW s@. You then get the weak
representation for free, using the same definition.

@
data A (s :: Strength) = A
  { a1 :: SW s Word8
  , a2 :: String }
@
-}
type family SW (s :: Strength) a :: Type where
    SW Strong a =          a
    SW Weak   a = Weakened a

-- | Shortcut for a 'SW'-wrapped 'SWChain'.
type SWN (s :: Strength) (n :: Natural) a = SW s (SWChain n a)
