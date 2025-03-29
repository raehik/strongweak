{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Strongweak.Incremental where

import Strongweak.Weaken
import GHC.TypeNats
import Data.Word
import Data.Functor.Identity
import Strongweak.Generic
import GHC.Generics
import Unsafe.Coerce
import Data.Type.Equality
import Data.Kind ( type Type )
import Data.Coerce

newtype Incremental (n :: Natural) a = Incremental
  { unIncremental :: WeakenedN n a }
deriving stock instance Show (WeakenedN n a) => Show (Incremental n a)

instance Weaken (WeakenedN n a) => Weaken (Incremental n a) where
    type Weakened (Incremental n a) = Incremental (n+1) a
    weaken a =
      case proveX @n @a of
        Refl -> Incremental (weaken (unIncremental a))

data B' (n :: Natural) = B'
  { b'1 :: Incremental n (Identity Word8)
  } deriving stock Generic
deriving instance Show (WeakenedN n (Identity Word8)) => Show (B' n)

instance Weaken (WeakenedN n (Identity Word8)) => Weaken (B' n) where
    type Weakened (B' n) = B' (n+1)
    weaken = weakenGeneric

type family Strip (b :: Bool) (n :: Natural) a where
    Strip False n a = Incremental n a
    Strip True  n a =   WeakenedN n a

data B'' (n :: Natural) (b :: Bool) = B''
  { b''1 :: Strip b n (Identity Word8)
  } deriving stock Generic
deriving instance Show (WeakenedN n (Identity Word8)) => Show (B'' n True)
deriving instance Show (WeakenedN n (Identity Word8)) => Show (B'' n False)

instance Weaken (WeakenedN n (Identity Word8)) => Weaken (B'' n False) where
    type Weakened (B'' n False) = B'' (n+1) False
    weaken = weakenGeneric

idkTF :: B'' n True -> B'' n False
idkTF = to . coerce . from

idkFT :: B'' n False -> B'' n True
idkFT = to . coerce . from

newtype Incremental' (n :: Natural) a = Incremental' { unIncremental' :: a }
    deriving stock Show

instance Weaken a => Weaken (Incremental' n a) where
    type Weakened (Incremental' n a) = Incremental' (n+1) (Weakened a)
    weaken = Incremental' . weaken . unIncremental'

data B (n :: Natural) = B
  { b1 :: Incremental' n (WeakenedN n (Identity Word8))
  } deriving stock Generic
deriving instance Show (Incremental' n (WeakenedN n (Identity Word8))) => Show (B n)

instance Weaken (WeakenedN n (Identity Word8)) => Weaken (B n) where
    type Weakened (B n) = B (n+1)
    weaken = case proveX @n @(Identity Word8) of Refl -> weakenGeneric

proveX :: forall n a. WeakenedN (n+1) a :~: Weakened (WeakenedN n a)
proveX = unsafeCoerce Refl
