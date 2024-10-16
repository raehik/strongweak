-- TODO what to name this? "coerce" has to be in it (that's the main thing).
-- also trivial? zero-invariant? what about the strategy?

-- TODO no longer really need thanks to SWChain. but maybe keep it around idk

module Strongweak.Coercibly where

import Strongweak.Weaken
import Strongweak.Strengthen
import Data.Coerce
import Data.Kind ( type Type )

{- | A @from@ that can be safely coerced between @to@.

You can use this to decide precisely how to weaken a newtype: whether to only
strip the newtype via 'Shallow', or to strip the newtype and weaken the inner
type via 'Deep'.
-}
newtype Coercibly (stg :: Strategy) (from :: Type) to
  = Coercibly { unCoercibly :: from }
    deriving stock Show

-- | How to weaken a layer type.
data Strategy
  = Shallow -- ^ Remove the layer.
  | Deep    -- ^ Remove the layer, and weaken the inner type.

-- note that without the Coercible instance, we get a confusing "couldn't match
-- representation of type 'from' with that of 'to'" error message. this might
-- happen in user code that tries to be parametric with 'Coercibly'

-- | Remove the coercible @from@ layer.
instance Coercible from to => Weaken (Coercibly Shallow from to) where
    type Weakened (Coercibly Shallow from to) = to
    weaken = coerce . unCoercibly

-- | Remove the coercible @from@ layer and weaken the result.
instance (Coercible from to, Weaken to) => Weaken (Coercibly Deep from to) where
    type Weakened (Coercibly Deep from to) = Weakened to
    weaken = weaken . coerce @from @to . unCoercibly

-- | An @f a@ that can be safely coerced between @a@.
newtype Coercibly1 (stg :: Strategy) f (a :: Type)
  = Coercibly1 { unCoercibly1 :: f a }
    deriving stock Show

-- | Remove the coercible @f a@ layer.
instance Coercible (f a) a => Weaken (Coercibly1 Shallow f a) where
    type Weakened (Coercibly1 Shallow f a) = a
    weaken = coerce . unCoercibly1

-- | Remove the coercible @f a@ layer and weaken the result.
instance (Coercible (f a) a, Weaken a) => Weaken (Coercibly1 Deep f a) where
    type Weakened (Coercibly1 Deep f a) = Weakened a
    weaken = weaken . coerce @(f a) @a . unCoercibly1


instance Coercible from to => Strengthen (Coercibly Shallow from to) where
    strengthen = Right . Coercibly . coerce @to @from

instance (Coercible from to, Strengthen to)
  => Strengthen (Coercibly Deep from to) where
    strengthen = fmap (Coercibly . coerce @to @from) <$> strengthen

instance Coercible (f a) a => Strengthen (Coercibly1 Shallow f a) where
    strengthen = Right . Coercibly1 . coerce @a @(f a)

instance (Coercible (f a) a, Strengthen a)
  => Strengthen (Coercibly1 Deep f a) where
    strengthen = fmap (Coercibly1 . coerce @a @(f a)) <$> strengthen
