{-# LANGUAGE UndecidableInstances #-} -- required due to nested constraints

module Strongweak.Generic.Via where

import Strongweak.Generic.Weaken
import Strongweak.Generic.Strengthen
import Strongweak
import GHC.Generics
import Data.Kind

{- | @DerivingVia@ wrapper for strongweak instances.

We can't use 'Generically' conveniently because we need to talk about two data
types, not one -- we would have to do something like @'Generically' ('Tagged' w
s)@, which is ugly. So we instead define our own adorable little "via type"
here!

Use like so:

@
data XYZ (s :: Strength) = XYZ
  { xyz1 :: SW s Word8
  , xyz2 :: Word8
  , xyz3 :: ()
  } deriving stock Generic
deriving via (GenericallySW (XYZ 'Strong) (XYZ 'Weak)) instance Weaken (XYZ 'Strong)
deriving via (GenericallySW (XYZ 'Strong) (XYZ 'Weak)) instance Strengthen (XYZ 'Strong)
@

TODO can't figure out a way around multiple standalone deriving declarations :(
-}

newtype GenericallySW s (w :: Type) = GenericallySW { unGenericallySW :: s }

instance
  ( Generic s, Generic w
  , GWeaken (Rep s) (Rep w)
  ) => Weaken (GenericallySW s w) where
    type Weak (GenericallySW s w) = w
    weaken = weakenGeneric . unGenericallySW

instance
  ( Generic s, Generic w
  , GStrengthenD (Rep w) (Rep s)
  , Weaken (GenericallySW s w)
  ) => Strengthen (GenericallySW s w) where
    strengthen = fmap GenericallySW . strengthenGeneric
