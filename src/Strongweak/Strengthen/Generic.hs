{- | 'strengthen' over generic representations.

As with base instances, generic strengthening collates all failures rather than
short-circuiting on the first failure. Failures are annotated with precise
information describing where the failure occurred:

  * data type name
  * constructor name
  * field index
  * field name (if present)
-}

-- required due to type class design
{-# LANGUAGE AllowAmbiguousTypes, UndecidableInstances #-}

{-# LANGUAGE OverloadedStrings #-} -- required for failure

module Strongweak.Strengthen.Generic where

import Data.Kind ( type Constraint )
import Strongweak.Strengthen
import GHC.Generics
import Data.Kind ( Type )
import GHC.TypeNats ( Natural, type (+), KnownNat )
import Strongweak.Util.TypeNats ( natVal'' )
import Data.Text.Builder.Linear qualified as TBL
import GHC.Exts ( Symbol, fromString, proxy# )
import GHC.TypeLits ( KnownSymbol, symbolVal' )

-- | Strengthen a value generically.
--
-- The weak and strong types must be /compatible/. See 'Strongweak.Generic' for
-- the definition of compatibility in this context.
strengthenGeneric
    :: (Generic w, Generic s, GStrengthenD (Rep w) (Rep s))
    => w -> Either StrengthenFailure' s
strengthenGeneric = fmap to . gstrengthenD . from

-- | Generic strengthening at the datatype level.
class GStrengthenD w s where
    gstrengthenD :: w p -> Either StrengthenFailure' (s p)

-- | Strengthen a generic data type, replacing its metadata wrapping.
instance GStrengthenC wdn sdn w s
  => GStrengthenD
        (D1 (MetaData wdn _wmd2 _wmd3 _wmd4) w)
        (D1 (MetaData sdn _smd2 _smd3 _smd4) s) where
    gstrengthenD = fmap M1 . gstrengthenC @wdn @sdn . unM1

-- | Generic strengthening at the constructor sum level.
class GStrengthenC (wdn :: Symbol) (sdn :: Symbol) w s where
    gstrengthenC :: w p -> Either StrengthenFailure' (s p)

-- | Nothing to do for empty datatypes.
instance GStrengthenC wdn sdn V1 V1 where gstrengthenC = Right

-- | Strengthen sum types by casing and strengthening left or right.
instance
  ( GStrengthenC wdn sdn wl sl
  , GStrengthenC wdn sdn wr sr
  ) => GStrengthenC wdn sdn (wl :+: wr) (sl :+: sr) where
    gstrengthenC = \case L1 l -> L1 <$> gstrengthenC @wdn @sdn l
                         R1 r -> R1 <$> gstrengthenC @wdn @sdn r

-- | Enter a constructor, stripping its metadata wrapper.
instance (GStrengthenS 0 w s, ReifyCstrs wcd wcn scd scn)
  => GStrengthenC wcd scd
        (C1 (MetaCons wcn _wmc2 _wmc3) w)
        (C1 (MetaCons scn _smc2 _smc3) s) where
    gstrengthenC (M1 w) =
        case gstrengthenS @0 w of
          Right s -> Right (M1 s)
          Left  e -> failStrengthen [reifyCstrs @wcd @wcn @scd @scn] e

type ReifyCstrs :: Symbol -> Symbol -> Symbol -> Symbol -> Constraint
class ReifyCstrs ld lc rd rc where reifyCstrs :: TBL.Builder

-- | Special case: data type and constructor names are equivalent: simplify
instance {-# OVERLAPPING #-} (KnownSymbol d, KnownSymbol c)
  => ReifyCstrs d c d c where
    {-# INLINE reifyCstrs #-}
    reifyCstrs = d<>"."<>c
      where
        d = fromString (symbolVal' (proxy# @d))
        c = fromString (symbolVal' (proxy# @c))

instance (KnownSymbol ld, KnownSymbol lc, KnownSymbol rd, KnownSymbol rc)
  => ReifyCstrs ld lc rd rc where
    {-# INLINE reifyCstrs #-}
    reifyCstrs = ld<>"."<>lc<>" -> "<>rd<>"."<>rc
      where
        ld = fromString (symbolVal' (proxy# @ld))
        lc = fromString (symbolVal' (proxy# @lc))
        rd = fromString (symbolVal' (proxy# @rd))
        rc = fromString (symbolVal' (proxy# @rc))

-- | Generic strengthening at the constructor level.
class GStrengthenS (i :: Natural) w s where
    gstrengthenS :: w p -> Either [(TBL.Builder, StrengthenFailure')] (s p)

-- | Nothing to do for empty constructors.
instance GStrengthenS i U1 U1 where gstrengthenS = Right

-- | Strengthen product types by strengthening left and right.
instance
  ( GStrengthenS i                  wl sl
  , GStrengthenS (i + ProdArity wl) wr sr
  ) => GStrengthenS i (wl :*: wr) (sl :*: sr) where
    gstrengthenS (l :*: r) =
        -- With @Validation@, @A.liftA2 (:*:)@. But I don't have a use for it
        -- elsewhere, so let's just be explicit.
        case gstrengthenS @i l of
          Right la ->
            case gstrengthenS @(i + ProdArity wl) r of
              Right  ra -> Right (la :*: ra)
              Left   re -> Left re
          Left  le ->
            case gstrengthenS @(i + ProdArity wl) @_ @sr r of
              Right _ra -> Left le
              Left   re -> Left (le <> re)

-- | Special case: if source and target types are equivalent, just replace meta.
--
-- Note that we have to expand the metadata awkwardly for the overlapping
-- instances to work correctly. (There should be a better way to write this, but
-- it's purely style, so light TODO.)
instance {-# OVERLAPPING #-} GStrengthenS i
  (S1 (MetaSel _wms1 _wms2 _wms3 _wms4) (Rec0 a))
  (S1 (MetaSel _sms1 _sms2 _sms3 _sms4) (Rec0 a)) where
    gstrengthenS = Right . M1 . unM1

-- | Strengthen a field using the existing 'Strengthen' instance.
instance
  ( Weak s ~ w -- has to be here, else "illegal typesym family app in instance"
  , Strengthen s
  , ReifySelector i wmr smr
  ) => GStrengthenS i
        (S1 (MetaSel wmr _wms2 _wms3 _wms4) (Rec0 w))
        (S1 (MetaSel smr _sms2 _sms3 _sms4) (Rec0 s)) where
    gstrengthenS = unM1 .> unK1 .> strengthen .> \case
      Right s -> Right $ M1 $ K1 s
      Left  e -> Left  [(reifySelector @i @wmr @smr, e)]

{- TODO
* how to separate index and record name? @.@ is good and bad, uses same syntax
  as @dt.cstr@ for different reason BUT is pretty clear
* how to lay out precisely? fairly arbitrary
-}
class ReifySelector (i :: Natural) (l :: Maybe Symbol) (r :: Maybe Symbol) where
    reifySelector :: TBL.Builder

-- | Special case: both types had a record name, and they're equal
instance {-# OVERLAPPING #-} (KnownNat i, KnownSymbol lnm)
  => ReifySelector i (Just lnm) (Just lnm) where
    -- TODO check overlap works correct
    {-# INLINE reifySelector #-}
    reifySelector = i<>"."<>lnm
      where
        i   = fromString $ show $ natVal'' @i
        lnm = fromString $ symbolVal' (proxy# @lnm)

instance (KnownNat i, KnownSymbol lnm, KnownSymbol rnm)
  => ReifySelector i (Just lnm) (Just rnm) where
    {-# INLINE reifySelector #-}
    reifySelector = i<>"."<>lnm<>" -> "<>rnm
      where
        i   = fromString $ show $ natVal'' @i
        lnm = fromString $ symbolVal' (proxy# @lnm)
        rnm = fromString $ symbolVal' (proxy# @rnm)

instance KnownNat i => ReifySelector i Nothing Nothing where
    {-# INLINE reifySelector #-}
    reifySelector = fromString $ show $ natVal'' @i

instance (KnownNat i, KnownSymbol lnm)
  => ReifySelector i (Just lnm) Nothing where
    {-# INLINE reifySelector #-}
    reifySelector = i<>"."<>lnm
      where
        i   = fromString $ show $ natVal'' @i
        lnm = fromString $ symbolVal' (proxy# @lnm)

instance (KnownNat i, KnownSymbol rnm)
  => ReifySelector i Nothing (Just rnm) where
    {-# INLINE reifySelector #-}
    reifySelector = i<>" -> "<>rnm
      where
        i   = fromString $ show $ natVal'' @i
        rnm = fromString $ symbolVal' (proxy# @rnm)

--------------------------------------------------------------------------------

-- from flow
(.>) :: (a -> b) -> (b -> c) -> a -> c
f .> g = g . f

--------------------------------------------------------------------------------

-- could define this with @Generic.Type.Function.FoldMap.GTFoldMapC (+) 0 _@...
-- but pretty dumb LOL
type family ProdArity (f :: k -> Type) :: Natural where
    ProdArity (S1 c f)  = 1
    ProdArity (l :*: r) = ProdArity l + ProdArity r
