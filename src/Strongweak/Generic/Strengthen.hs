{- | 'strengthen' over generic representations.

Strengthen failures are annotated with precise information describing where the
failure occurred: datatype name, constructor name, field index (and name if
present). To achieve this, we split the generic derivation into 3 classes, each
handling/"unwrapping" a different layer of the generic representation: datatype
(D), constructor (C) and selector (S).
-}

-- both required due to type class design
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Strongweak.Generic.Strengthen where

import Strongweak.Strengthen
import Data.Either.Validation
import GHC.Generics
import Data.Kind
import GHC.TypeNats
import GHC.Exts ( proxy#, Proxy# )

import Control.Applicative ( liftA2 ) -- required for older GHCs

-- | Strengthen a value generically.
--
-- The weak and strong types must be /compatible/. See 'Strongweak.Generic' for
-- the definition of compatibility in this context.
strengthenGeneric
    :: (Generic w, Generic s, GStrengthenD (Rep w) (Rep s))
    => w -> Result s
strengthenGeneric = fmap to . gstrengthenD . from

-- | Generic strengthening at the datatype level.
class GStrengthenD w s where
    gstrengthenD :: w p -> Result (s p)

-- | Enter a datatype, stripping its metadata wrapper.
instance GStrengthenC wcd scd w s => GStrengthenD (D1 wcd w) (D1 scd s) where
    gstrengthenD = fmap M1 . gstrengthenC @wcd @scd . unM1

-- | Generic strengthening at the constructor sum level.
class GStrengthenC wcd scd w s where
    gstrengthenC :: w p -> Result (s p)

-- | Nothing to do for empty datatypes.
instance GStrengthenC wcd scd V1 V1 where gstrengthenC = Success

-- | Strengthen sum types by casing and strengthening left or right.
instance
  ( GStrengthenC wcd scd wl sl
  , GStrengthenC wcd scd wr sr
  ) => GStrengthenC wcd scd (wl :+: wr) (sl :+: sr) where
    gstrengthenC = \case L1 l -> L1 <$> gstrengthenC @wcd @scd l
                         R1 r -> R1 <$> gstrengthenC @wcd @scd r

-- | Enter a constructor, stripping its metadata wrapper.
instance GStrengthenS wcd scd wcc scc 0 w s
  => GStrengthenC wcd scd (C1 wcc w) (C1 scc s) where
    gstrengthenC = fmap M1 . gstrengthenS @wcd @scd @wcc @scc @0 . unM1

-- | Generic strengthening at the constructor level.
class GStrengthenS wcd scd wcc scc (si :: Natural) w s where
    gstrengthenS :: w p -> Result (s p)

-- | Nothing to do for empty constructors.
instance GStrengthenS wcd scd wcc scc si U1 U1 where gstrengthenS = Success

-- | Strengthen product types by strengthening left and right.
instance
  ( GStrengthenS wcd scd wcc scc si                  wl sl
  , GStrengthenS wcd scd wcc scc (si + ProdArity wl) wr sr
  ) => GStrengthenS wcd scd wcc scc si (wl :*: wr) (sl :*: sr) where
    gstrengthenS (l :*: r) =
        liftA2 (:*:)
               (gstrengthenS @wcd @scd @wcc @scc @si                  l)
               (gstrengthenS @wcd @scd @wcc @scc @(si + ProdArity wl) r)

-- | Special case: if source and target types are equal, copy the value through.
instance GStrengthenS wcd scd wcc scc si (S1 wcs (Rec0 a)) (S1 scs (Rec0 a)) where
    gstrengthenS = Success . M1 . unM1

-- | Strengthen a field using the existing 'Strengthen' instance.
instance {-# OVERLAPS #-}
  ( Weak s ~ w -- has to be here, else "illegal typesym family app in instance"
  , Strengthen s
  , Datatype wcd, Datatype scd
  , Constructor wcc, Constructor scc
  , Selector wcs, Selector scs
  , KnownNat si
  ) => GStrengthenS wcd scd wcc scc si (S1 wcs (Rec0 w)) (S1 scs (Rec0 s)) where
    gstrengthenS = unM1 .> unK1 .> strengthen .> \case
      Success s  -> Success $ M1 $ K1 s
      Failure es -> Failure $ pure e
        where
          e = FailField wcd scd wcc scc si wcs si scs es
          wcd = datatypeName' @wcd
          scd = datatypeName' @scd
          wcc = conName' @wcc
          scc = conName' @scc
          wcs = selName'' @wcs
          scs = selName'' @scs
          si = natVal'' @si

--------------------------------------------------------------------------------

-- from flow
(.>) :: (a -> b) -> (b -> c) -> a -> c
f .> g = g . f

--------------------------------------------------------------------------------

-- | Get the record name for a selector if present.
--
-- On the type level, a 'Maybe Symbol' is stored for record names. But the
-- reification is done using @fromMaybe ""@. So we have to inspect the resulting
-- string to determine whether the field uses record syntax or not. (Silly.)
selName'' :: forall s. Selector s => Maybe String
selName'' = case selName' @s of "" -> Nothing
                                s  -> Just s

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

--------------------------------------------------------------------------------

type family ProdArity (f :: Type -> Type) :: Natural where
    ProdArity (S1 c f)  = 1
    ProdArity (l :*: r) = ProdArity l + ProdArity r

--------------------------------------------------------------------------------

natVal'' :: forall n. KnownNat n => Natural
natVal'' = natVal' (proxy# :: Proxy# n)
{-# INLINE natVal'' #-}
