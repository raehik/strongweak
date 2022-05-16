module Strongweak.Example where

import Strongweak
import Strongweak.Generic

import GHC.Generics ( Generic )

import Data.Word ( Word8 )

import Refined hiding ( Weaken(..) )
import Numeric.Natural

data Ex1D (s :: Strength) = Ex1C
  { ex1f1 :: SW s Word8
  , ex1f2 :: SW s (Refined (LessThan 100) Natural)
  } deriving stock Generic
deriving stock instance Show (Ex1D 'Strong)
deriving stock instance Show (Ex1D 'Weak)
instance Weaken     (Ex1D 'Strong) (Ex1D 'Weak)   where weaken     = weakenGeneric
instance Strengthen (Ex1D 'Weak)   (Ex1D 'Strong) where strengthen = strengthenGeneric

data Ex2D (s :: Strength) = Ex2C
  { ex2f1 :: Ex1D s
  , ex2f2 :: SW s Word8
  } deriving stock Generic
deriving stock instance Show (Ex2D 'Strong)
deriving stock instance Show (Ex2D 'Weak)
instance Weaken     (Ex2D 'Strong) (Ex2D 'Weak)   where weaken     = weakenGeneric
instance Strengthen (Ex2D 'Weak)   (Ex2D 'Strong) where strengthen = strengthenGeneric

ex1w :: Ex1D 'Weak
ex1w = Ex1C 256 210

ex2w :: Ex2D 'Weak
ex2w = Ex2C ex1w 256

data ExVoid (s :: Strength) deriving stock Generic
instance Weaken     (ExVoid 'Strong) (ExVoid 'Weak)   where weaken     = weakenGeneric
instance Strengthen (ExVoid 'Weak)   (ExVoid 'Strong) where strengthen = strengthenGeneric

data ExUnit (s :: Strength) = ExUnit deriving stock Generic
instance Weaken     (ExUnit 'Strong) (ExUnit 'Weak)   where weaken     = weakenGeneric
instance Strengthen (ExUnit 'Weak)   (ExUnit 'Strong) where strengthen = strengthenGeneric
