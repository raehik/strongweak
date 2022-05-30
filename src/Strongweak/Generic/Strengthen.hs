{- | Strengthening for generic data types.

The generic derivation is split into 3 classes, each dealing with a different
layer of a generic Haskell data type: datatype (D), constructor (C) and selector
(S). At each point, we gather up information about the type and push on.
Strengthening occurs at selectors. If a strengthening fails, the gathered
information is pushed into an error that wraps the original error.
-}

{-# LANGUAGE AllowAmbiguousTypes #-}

module Strongweak.Generic.Strengthen where

import Strongweak.Strengthen
import Data.Validation
import Data.List.NonEmpty
import GHC.Generics

import Numeric.Natural
import Control.Applicative ( liftA2 )

strengthenGeneric
    :: (Generic w, Generic s, GStrengthenD (Rep w) (Rep s))
    => w -> Validation (NonEmpty StrengthenError) s
strengthenGeneric = fmap to . gstrengthenD . from

class GStrengthenD w s where
    gstrengthenD :: w p -> Validation (NonEmpty StrengthenError) (s p)

instance (GStrengthenC w s, Datatype dw, Datatype ds) => GStrengthenD (D1 dw w) (D1 ds s) where
    gstrengthenD = fmap M1 . gstrengthenC (datatypeName' @dw) (datatypeName' @ds) . unM1

class GStrengthenC w s where
    gstrengthenC :: String -> String -> w p -> Validation (NonEmpty StrengthenError) (s p)

-- | Nothing to do for empty datatypes.
instance GStrengthenC V1 V1 where
    gstrengthenC _ _ = Success

instance (GStrengthenS w s, Constructor cw, Constructor cs) => GStrengthenC (C1 cw w) (C1 cs s) where
    gstrengthenC dw ds = fmap M1 . snd . gstrengthenS dw ds (conName' @cw) (conName' @cs) 0 . unM1

-- | Strengthen sum types by strengthening left or right.
instance (GStrengthenC lw ls, GStrengthenC rw rs) => GStrengthenC (lw :+: rw) (ls :+: rs) where
    gstrengthenC dw ds = \case L1 l -> L1 <$> gstrengthenC dw ds l
                               R1 r -> R1 <$> gstrengthenC dw ds r

class GStrengthenS w s where
    gstrengthenS :: String -> String -> String -> String -> Natural -> w p -> (Natural, Validation (NonEmpty StrengthenError) (s p))

-- | Nothing to do for empty constructors.
instance GStrengthenS U1 U1 where
    gstrengthenS _ _ _ _ n x = (n, Success x)

-- | Special case: if source and target types are equal, copy the value through.
instance GStrengthenS (S1 mw (Rec0 w)) (S1 ms (Rec0 w)) where
    gstrengthenS _ _ _ _ n x = (n, Success (M1 (unM1 x)))

-- | Strengthen a field using the existing 'Strengthen' instance.
--
-- On strengthen failure, the errors are annotated with all the datatype
-- information we've hoarded. The upshot is that if you strengthen a type with
-- lots of types inside it, all with generically-derived 'Strengthen' instances,
-- you'll get a precise zoom-in of exactly where each error occurred.
instance {-# OVERLAPS #-} (Strengthen w s, Selector mw, Selector ms) => GStrengthenS (S1 mw (Rec0 w)) (S1 ms (Rec0 s)) where
    gstrengthenS dw ds cw cs n (M1 (K1 w)) =
        case strengthen w of
          Failure es ->
            let fw = selName'' @mw
                fs = selName'' @ms
                e  = StrengthenErrorField dw ds cw cs n fw n fs es
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

-- | Strengthen product types by strengthening left and right.
--
-- This is ordered (left then right), but only to pass the index along.
instance (GStrengthenS lw ls, GStrengthenS rw rs) => GStrengthenS (lw :*: rw) (ls :*: rs) where
    gstrengthenS dw ds cw cs n (l :*: r) = (n'', liftA2 (:*:) l' r')
      where
        (n',  l') = gstrengthenS dw ds cw cs n      l
        (n'', r') = gstrengthenS dw ds cw cs (n'+1) r

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
