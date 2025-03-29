{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Strongweak.Example where

import Strongweak
import Strongweak.Generic
import Strongweak.Weaken
import Data.Word
import Data.Functor.Identity
import GHC.TypeNats
import GHC.Generics
import Strongweak.Weaken.GenericN
import Unsafe.Coerce
import Data.Type.Equality

data A (n :: Natural) = A { a1 :: WeakenedN n (Identity Word8) }
    deriving stock Generic
deriving stock instance Show (A 0)
deriving stock instance Show (A 1)
deriving stock instance Show (A 2)
--deriving stock instance Show (A 3)
{-
instance Weaken (A n) where
    type Weakened (A n) = A (n+1)
    weaken = (withWeakenedNPlus @n @(Identity Word8)) weakenGeneric
instance Weaken (A 0) where
    type Weakened (A 0) = A 1
    weaken = weakenGeneric
instance Weaken (A 1) where
    type Weakened (A 1) = A 2
    weaken = weakenGeneric

withWeakenedNPlus
    :: forall n a r
    .  (forall x. WeakenedN (n+1) x ~ Weakened (WeakenedN n x) => a -> r)
    -> a -> r
withWeakenedNPlus f a =
    case proveX @n @a of
      Refl -> f a

proveX :: forall n a. WeakenedN (n+1) a :~: Weakened (WeakenedN n a)
proveX = unsafeCoerce Refl
-}
