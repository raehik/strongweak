module Common where

import Strongweak
import Strongweak.Generic
import Refined hiding ( Weaken, weaken, strengthen, NonEmpty )
import GHC.Generics ( Generic )
import Generic.Random
import Test.QuickCheck ( Arbitrary )
import Numeric.Natural ( Natural )
import Data.Word
import Test.QuickCheck.Instances.Natural()
import Data.Either.Validation

data DS (s :: Strength)
  = DS0 (SW s Word8) (SW s Word8) Word8 (SW s Word8) (SW s Word8)
  | DS1 (SW s (Refined (LessThan 100) Natural))
    deriving stock (Generic)

deriving stock instance Eq   (DS 'Strong)
deriving stock instance Show (DS 'Strong)
deriving via (GenericArbitraryU `AndShrinking` (DS 'Strong)) instance Arbitrary (DS 'Strong)

deriving stock instance Eq   (DS 'Weak)
deriving stock instance Show (DS 'Weak)
deriving via (GenericArbitraryU `AndShrinking` (DS 'Weak))   instance Arbitrary (DS 'Weak)

instance Weaken (DS 'Strong) where
    type Weak   (DS 'Strong) = DS 'Weak
    weaken = weakenGeneric
instance Strengthen (DS 'Strong) where strengthen = strengthenGeneric

data DP (s :: Strength) = DP
  { dp1f0 :: SW s Word32
  , dp1f1 :: SW s (Refined (GreaterThan 42) Natural)
  , dp1f2 :: SW s Word8
  , dp1f3 :: Word8
  , dp1f4 :: SW s Word8
  } deriving stock (Generic)

deriving stock instance Eq   (DP 'Strong)
deriving stock instance Show (DP 'Strong)
deriving via (GenericArbitraryU `AndShrinking` (DP 'Strong)) instance Arbitrary (DP 'Strong)

deriving stock instance Eq   (DP 'Weak)
deriving stock instance Show (DP 'Weak)
deriving via (GenericArbitraryU `AndShrinking` (DP 'Weak))   instance Arbitrary (DP 'Weak)

instance Weaken     (DP 'Strong) where
    type Weak (DP 'Strong) = DP 'Weak
    weaken = weakenGeneric
instance Strengthen (DP 'Strong) where strengthen = strengthenGeneric

tryStrengthenSuccessEq :: Eq a => a -> TryStrengthen a -> Bool
tryStrengthenSuccessEq a = \case Success a' -> a == a'; Failure{} -> False
