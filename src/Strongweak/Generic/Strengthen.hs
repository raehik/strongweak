{- |
The generic derivation is split into 3 classes, dealing with different layers of
a Haskell data type: datatype, constructor and selector. At each point, we
gather up information about the type and push on. Strengthening occurs at
selectors. If a strengthening fails, the gathered information is pushed into an
error that wraps the original error.
-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}

module Strongweak.Generic.Strengthen where

import Strongweak.Strengthen
import Data.Validation
import Data.List.NonEmpty

import GHC.Generics

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
    gstrengthenC dw ds = fmap M1 . gstrengthenS dw ds (conName' @cw) (conName' @cs) . unM1

-- | Strengthen sum types by strengthening left or right.
instance (GStrengthenC lw ls, GStrengthenC rw rs) => GStrengthenC (lw :+: rw) (ls :+: rs) where
    gstrengthenC dw ds = \case L1 l -> L1 <$> gstrengthenC dw ds l
                               R1 r -> R1 <$> gstrengthenC dw ds r

class GStrengthenS w s where
    gstrengthenS :: String -> String -> String -> String -> w p -> Validation (NonEmpty StrengthenError) (s p)

-- | Nothing to do for empty constructors.
instance GStrengthenS U1 U1 where
    gstrengthenS _ _ _ _ = Success

-- | Special case: if source and target types are equal, copy the value through.
instance GStrengthenS (S1 mw (Rec0 w)) (S1 ms (Rec0 w)) where
    gstrengthenS _ _ _ _ = Success . M1 . unM1

-- | Strengthen a field using the existing 'Strengthen' instance.
instance {-# OVERLAPS #-} (Strengthen w s, Selector mw, Selector ms) => GStrengthenS (S1 mw (Rec0 w)) (S1 ms (Rec0 s)) where
    gstrengthenS dw ds cw cs (M1 (K1 w)) =
        case strengthen w of
          Failure (e :| es) -> Failure $ StrengthenErrorField dw ds cw cs (selName' @mw) (selName' @ms) e :| es
          Success s   -> Success $ M1 $ K1 s

-- | Strengthen product types by strengthening left, then right.
instance (GStrengthenS lw ls, GStrengthenS rw rs) => GStrengthenS (lw :*: rw) (ls :*: rs) where
    gstrengthenS dw ds cw cs (l :*: r) = do
        l' <- gstrengthenS dw ds cw cs l
        r' <- gstrengthenS dw ds cw cs r
        return $ l' :*: r'

--------------------------------------------------------------------------------

-- | 'conName' without the value (only used as a proxy). Lets us push our
--   'undefined's into one place.
conName' :: forall c. Constructor c => String
conName' = conName @c undefined

-- | 'datatypeName' without the value (only used as a proxy). Lets us push our
--   'undefined's into one place.
datatypeName' :: forall d. Datatype d => String
datatypeName' = datatypeName @d undefined

-- | 'datatypeName' without the value (only used as a proxy). Lets us push our
--   'undefined's into one place.
selName' :: forall s. Selector s => String
selName' = selName @s undefined
