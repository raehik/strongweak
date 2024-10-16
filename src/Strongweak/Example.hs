module Strongweak.Example where
import Strongweak
import Strongweak.Generic
import GHC.Generics ( Generic )
import Data.List.NonEmpty as NE
import Data.Word

data A (s :: Strength) = A
  { a1 :: SW s (NE.NonEmpty (SW s Word8))
  } deriving stock Generic
deriving instance Show (A Weak)
deriving instance Show (A Strong)

instance Weaken (A Strong) where
    type Weakened (A Strong) = A Weak
    weaken = undefined -- weakenGeneric
