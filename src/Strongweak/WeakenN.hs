module Strongweak.WeakenN ( WeakenN(..) ) where

import Strongweak.WeakenN.Internal
import Strongweak.Weaken ( Weaken(..), type WeakenedN )
import Strongweak.Strengthen ( Strengthen(..) )
import GHC.TypeNats ( type Natural )

{- | When weakening (or strengthening), chain the operation @n@ times.

You may achieve this without extra newtypes by nesting uses of
'Strongweak.Weaken.SW'. However, strongweak generics can't handle this, forcing
you to write manual instances.

'WeakenN' provides this nesting behaviour in a type. In return for adding a
boring newtype layer to the strong representation, you can chain weakening and
strengthenings without having to write them manually.

The type works as follows:

@
Weakened (WeakenN 0 a) = a
Weakened (WeakenN 1 a) = Weakened a
Weakened (WeakenN 2 a) = Weakened (Weakened a)
Weakened (WeakenN n a) = WeakenedN n a
@

And so on. (This type is only much use from @n = 2@ onwards.)
-}
newtype WeakenN (n :: Natural) a = WeakenN { unWeakenN :: a }
    deriving stock Show

instance WeakenWeakenN n a => Weaken (WeakenN n a) where
    type Weakened (WeakenN n a) = WeakenedN n a
    weaken = weakenWeakenN @n @a . unWeakenN

instance StrengthenWeakenN n a => Strengthen (WeakenN n a) where
    strengthen = fmap WeakenN . strengthenWeakenN @n @a
