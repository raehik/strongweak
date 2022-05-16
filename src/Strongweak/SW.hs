module Strongweak.SW where

import Refined ( Refined )
import Data.Vector.Sized ( Vector )
import Data.Kind ( Type )
import Data.Word
import Data.Int
import Numeric.Natural ( Natural )

data Strength = Strong | Weak

-- | Obtain the weak representation of the given type.
type family Weak (a :: Type) :: Type

-- machine integers
type instance Weak Word8  = Natural
type instance Weak Word16 = Natural
type instance Weak Word32 = Natural
type instance Weak Word64 = Natural
type instance Weak Int8   = Integer
type instance Weak Int16  = Integer
type instance Weak Int32  = Integer
type instance Weak Int64  = Integer

-- other
type instance Weak (Vector n a) = [a]
type instance Weak (Refined p a) = a

{- |
Obtain either the strong or weak representation of a type, depending on the
type-level strength "switch" provided.

This is intended to be used in data types that take a 'Strength' type. Define
your type using strong fields wrapped in @Switch s@. You then get the weak
representation for free, using the same definition.

@
data A (s :: Strength) = A
  { aField1 :: Switch s Word8
  , aField2 :: String }
@
-}
type family SW (s :: Strength) a :: Type where
    SW 'Strong a = a
    SW 'Weak   a = Weak a
