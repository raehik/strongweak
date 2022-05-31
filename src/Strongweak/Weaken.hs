module Strongweak.Weaken where

import Refined ( Refined, unrefine )
import Numeric.Natural ( Natural )
import Data.Word
import Data.Int
import Data.Vector.Sized qualified as Vector
import Data.Vector.Sized ( Vector )
import Data.Functor.Identity
import Data.Functor.Const

{- | Any 's' can be "weakened" into a 'w'.

For example, you may weaken a 'Word8' into a 'Natural'.

Note that we restrict strengthened types to having only one corresponding weak
representation using functional dependencies.
-}
class Weaken s w where weaken :: s -> w

instance Weaken a (Maybe a) where weaken = Just
instance Weaken a (Either e a) where weaken = Right

instance Weaken a (Identity a) where weaken = Identity
instance Weaken a (Const a b) where weaken = Const

-- | Weaken each element of a functor. TODO
instance (Weaken s w, Functor f) => Weaken (f s) (f w) where weaken = fmap weaken

-- | Weaken sized vectors into plain lists.
instance Weaken (Vector n a) [a] where weaken = Vector.toList

-- | Strip the refinement from refined types.
instance Weaken (Refined p a) a where weaken = unrefine

-- Weaken the bounded Haskell numeric types using 'fromIntegral'.
instance Weaken Word8  Natural where weaken = fromIntegral
instance Weaken Word16 Natural where weaken = fromIntegral
instance Weaken Word32 Natural where weaken = fromIntegral
instance Weaken Word64 Natural where weaken = fromIntegral
instance Weaken Int8   Integer where weaken = fromIntegral
instance Weaken Int16  Integer where weaken = fromIntegral
instance Weaken Int32  Integer where weaken = fromIntegral
instance Weaken Int64  Integer where weaken = fromIntegral
