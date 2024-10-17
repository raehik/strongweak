module Strongweak.Chain ( SWChain(..), strengthenN ) where

import Strongweak.Weaken ( Weaken(..), WeakenN(weakenN), type WeakenedN )
import Strongweak.Strengthen ( Strengthen(..), StrengthenN(strengthenN) )
import GHC.TypeNats ( type Natural )

{- | When weakening (or strengthening), chain the operation @n@ times.

You may achieve this without extra newtypes by nesting uses of
'Strongweak.Strength.SW'. However, strongweak generics can't handle this,
forcing you to write manual instances.

'SWChain' provides this nesting behaviour in a type. In return for adding a
boring newtype layer to the strong representation, you can chain weakening and
strengthenings without having to write them manually.

The type works as follows:

@
'Weakened' ('SWChain' 0 a) = a
'Weakened' ('SWChain' 1 a) = 'Weakened' a
'Weakened' ('SWChain' 2 a) = 'Weakened' ('Weakened' a)
'Weakened' ('SWChain' n a) = 'WeakenedN' n a
@

And so on. (This type is only much use from @n = 2@ onwards.)

You may also use this as a "via" type:

@
newtype A (s :: 'Strength') = A { a1 :: 'SW' s (Identity ('SW' s Word8)) }
deriving via 'SWChain' 2 (Identity Word8) instance     'Weaken' (A 'Strong')
deriving via 'SWChain' 2 (Identity Word8) instance 'Strengthen' (A 'Strong')
@
-}
newtype SWChain (n :: Natural) a = SWChain { unSWChain :: a }
    deriving stock Show
    deriving (Ord, Eq) via a

instance WeakenN n a => Weaken (SWChain n a) where
    type Weakened (SWChain n a) = WeakenedN n a
    weaken = weakenN @n . unSWChain

instance StrengthenN n a => Strengthen (SWChain n a) where
    strengthen = fmap SWChain . strengthenN @n
