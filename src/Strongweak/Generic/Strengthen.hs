{- | 'strengthen' over generic representations.

Strengthen failures are annotated with precise information describing where the
failure occurred: datatype name, constructor name, field index (and name if
present). To achieve this, we split the generic derivation into 3 classes, each
handling/"unwrapping" a different layer of the generic representation: datatype
(D), constructor (C) and selector (S).
-}

{-# LANGUAGE AllowAmbiguousTypes #-}

module Strongweak.Generic.Strengthen where

import Strongweak.Strengthen
import Data.Either.Validation
import Data.List.NonEmpty
import GHC.Generics

import Numeric.Natural
import Control.Applicative ( liftA2 )

-- | Strengthen a value generically.
--
-- The weak and strong types must be /compatible/. See 'Strongweak.Generic' for
-- the definition of compatibility in this context.
strengthenGeneric
    :: (Generic w, Generic s, GStrengthenD (Rep w) (Rep s))
    => w -> Validation (NonEmpty StrengthenFail) s
strengthenGeneric = fmap to . gstrengthenD . from

-- | Generic strengthening at the datatype level.
class GStrengthenD w s where
    gstrengthenD :: w p -> Validation (NonEmpty StrengthenFail) (s p)

-- | Enter a datatype, stripping its metadata wrapper.
instance (GStrengthenC w s, Datatype dw, Datatype ds) => GStrengthenD (D1 dw w) (D1 ds s) where
    gstrengthenD = fmap M1 . gstrengthenC (datatypeName' @dw) (datatypeName' @ds) . unM1

-- | Generic strengthening at the constructor sum level.
class GStrengthenC w s where
    gstrengthenC :: String -> String -> w p -> Validation (NonEmpty StrengthenFail) (s p)

-- | Nothing to do for empty datatypes.
instance GStrengthenC V1 V1 where
    gstrengthenC _ _ = Success

-- | Strengthen sum types by casing and strengthening left or right.
instance (GStrengthenC lw ls, GStrengthenC rw rs) => GStrengthenC (lw :+: rw) (ls :+: rs) where
    gstrengthenC dw ds = \case L1 l -> L1 <$> gstrengthenC dw ds l
                               R1 r -> R1 <$> gstrengthenC dw ds r

-- | Enter a constructor, stripping its metadata wrapper.
instance (GStrengthenS w s, Constructor cw, Constructor cs) => GStrengthenC (C1 cw w) (C1 cs s) where
    gstrengthenC dw ds = fmap M1 . snd . gstrengthenS dw ds (conName' @cw) (conName' @cs) 0 . unM1

{- | Generic strengthening at the selector product level.

In order to calculate field indices, we return the current field index alongside
the result. This way, the product case can strengthen the left branch, then
increment the returned field index and use it for strengthening the right
branch.
-}
class GStrengthenS w s where
    gstrengthenS
        :: String  -- ^ weak   datatype name
        -> String  -- ^ strong datatype name
        -> String  -- ^ weak   constructor name
        -> String  -- ^ strong constructor name
        -> Natural -- ^ current field index (0, from left)
        -> w p -> (Natural, Validation (NonEmpty StrengthenFail) (s p))

-- | Nothing to do for empty constructors.
instance GStrengthenS U1 U1 where
    gstrengthenS _ _ _ _ n x = (n, Success x)

-- | Strengthen product types by strengthening left and right.
--
-- This is ordered (left then right) in order to pass the field index along.
instance (GStrengthenS lw ls, GStrengthenS rw rs) => GStrengthenS (lw :*: rw) (ls :*: rs) where
    gstrengthenS dw ds cw cs n (l :*: r) = (n'', liftA2 (:*:) l' r')
      where
        (n',  l') = gstrengthenS dw ds cw cs n      l
        (n'', r') = gstrengthenS dw ds cw cs (n'+1) r

-- | Special case: if source and target types are equal, copy the value through.
instance GStrengthenS (S1 mw (Rec0 w)) (S1 ms (Rec0 w)) where
    gstrengthenS _ _ _ _ n x = (n, Success (M1 (unM1 x)))

-- | Strengthen a field using the existing 'Strengthen' instance.
instance {-# OVERLAPS #-} (Strengthen s, Weak s ~ w, Selector mw, Selector ms) => GStrengthenS (S1 mw (Rec0 w)) (S1 ms (Rec0 s)) where
    gstrengthenS dw ds cw cs n (M1 (K1 w)) =
        case strengthen w of
          Failure es ->
            let fw = selName'' @mw
                fs = selName'' @ms
                e  = StrengthenFailField dw ds cw cs n fw n fs es
            in  (n, Failure $ e :| [])
          Success s   -> (n, Success $ M1 $ K1 s)

-- | Get the record name for a selector if present.
--
-- On the type level, a 'Maybe Symbol' is stored for record names. But the
-- reification is done using @fromMaybe ""@. So we have to inspect the resulting
-- string to determine whether the field uses record syntax or not. (Silly.)
selName'' :: forall s. Selector s => Maybe String
selName'' = case selName' @s of "" -> Nothing
                                s  -> Just s

--------------------------------------------------------------------------------

-- | 'conName' without the value (only used as a proxy). Lets us push our
--   'undefined's into one place.
conName' :: forall c. Constructor c => String
conName' = conName @c undefined

-- | 'datatypeName' without the value (only used as a proxy). Lets us push our
--   'undefined's into one place.
datatypeName' :: forall d. Datatype d => String
datatypeName' = datatypeName @d undefined

-- | 'selName' without the value (only used as a proxy). Lets us push our
--   'undefined's into one place.
selName' :: forall s. Selector s => String
selName' = selName @s undefined
