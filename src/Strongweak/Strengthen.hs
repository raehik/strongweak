{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Strongweak.Strengthen
  (
  -- * 'Strengthen' class
    Strengthen(..)
  , restrengthen
  , Result

  -- ** Helpers
  , strengthenBounded

  -- * Strengthen failures
  , Fails
  , Fail(..)
  , prettyFail

  -- ** Helpers
  , fail1
  , failOther
  , failShow
  , maybeFailShow

  -- * Re-exports
  , Strongweak.Weaken.Weak
  ) where

import Strongweak.Util.Typeable ( typeRep' )
import Strongweak.Util.Text ( tshow )
import Strongweak.Weaken ( Weaken(..) )
import Data.Either.Validation
import Data.Typeable ( Typeable, TypeRep )
import Prettyprinter qualified as Pretty
import Prettyprinter ( Pretty(pretty), (<+>) )
import Prettyprinter.Render.String qualified as Pretty
import Prettyprinter.Render.Text qualified as Pretty

import Data.Text ( Text )
import Data.Text.Lazy qualified as Text.Lazy
import GHC.TypeNats ( Natural, KnownNat )
import Data.Word
import Data.Int
import Rerefined
import Data.Vector.Generic.Sized qualified as VGS -- Shazbot!
import Data.Vector.Generic qualified as VG
import Data.Foldable qualified as Foldable
import Control.Applicative ( liftA2 ) -- required for older GHCs
import Data.Functor.Identity
import Data.Functor.Const
import Acc.NeAcc
import Data.List.NonEmpty qualified as NonEmpty
import Data.List.NonEmpty ( NonEmpty )

type Result = Validation Fails
type Fails = NeAcc Fail

{- | Attempt to strengthen some @'Weak' a@, asserting certain invariants.

We take 'Weaken' as a superclass in order to maintain strong/weak type pair
consistency. We choose this dependency direction because we treat the strong
type as the "canonical" one, so 'Weaken' is the more natural (and
straightforward) class to define. That does mean the instances for this class
are a little confusingly worded. Alas.

See "Strongweak" for class design notes and laws.
-}
class Weaken a => Strengthen a where
    -- | Attempt to strengthen some @'Weak' a@ to its associated strong type
    --   @a@.
    strengthen :: Weak a -> Result a

-- | Weaken a strong value, then strengthen it again.
--
-- Potentially useful if you have previously used
-- 'Strongweak.Strengthen.Unsafe.unsafeStrengthen' and now wish to check the
-- invariants. For example:
--
-- >>> restrengthen $ unsafeStrengthen @(Vector 2 Natural) [0]
-- Failure ...
restrengthen
    :: (Strengthen a, Weaken a)
    => a -> Result a
restrengthen = strengthen . weaken

-- | A failure encountered during strengthening.
data Fail
  -- | A failure containing lots of detail. Use in concrete instances where you
  --   already have the 'Show's and 'Typeable's needed.
  = FailShow
        TypeRep -- ^ weak   type
        TypeRep -- ^ strong type
        (Maybe Text) -- ^ weak value
        [Text] -- ^ failure description

  -- | A failure. Use in abstract instances to avoid heavy contexts. (Remember
  --   that generic strengtheners should wrap these nicely anyway!)
  | FailOther
        [Text] -- ^ failure description

  -- | Some failures occurred when strengthening from one data type to another.
  --
  -- Field indices are from 0 in the respective constructor. Field names are
  -- provided if are present in the type.
  --
  -- This is primarily intended to be used by generic strengtheners.
  | FailField
        String                      -- ^ weak   datatype name
        String                      -- ^ strong datatype name
        String                      -- ^ weak   constructor name
        String                      -- ^ strong constructor name
        Natural                     -- ^ weak   field index
        (Maybe String)              -- ^ weak   field name (if present)
        Natural                     -- ^ strong field index
        (Maybe String)              -- ^ strong field name (if present)
        Fails                       -- ^ failures

prettyFail :: Fail -> Text.Lazy.Text
prettyFail = Pretty.renderLazy . prettyLayoutFail

prettyLayoutFail :: Fail -> Pretty.SimpleDocStream ann
prettyLayoutFail = Pretty.layoutPretty Pretty.defaultLayoutOptions . pretty

fail1 :: Fail -> Result a
fail1 = Failure . pure

failOther :: [Text] -> Result a
failOther = fail1 . FailOther

buildFailShow
    :: forall w s. (Typeable w, Typeable s)
    => Maybe Text -> [Text] -> Result s
buildFailShow mwv = fail1 . FailShow (typeRep' @w) (typeRep' @s) mwv

failShow'
    :: forall s w. (Typeable w, Show w, Typeable s)
    => (w -> Text) -> w -> [Text] -> Result s
failShow' f w = buildFailShow @w @s (Just (f w))

failShow
    :: forall s w. (Typeable w, Show w, Typeable s)
    => w -> [Text] -> Result s
failShow = failShow' tshow

-- | This reports the weak and strong type, so no need to include those in the
--   failure detail.
failShowNoVal :: forall w s. (Typeable w, Typeable s) => [Text] -> Result s
failShowNoVal = buildFailShow @w @s Nothing

instance Show Fail where
    showsPrec _ = Pretty.renderShowS . prettyLayoutFail

-- TODO shorten value if over e.g. 50 chars. e.g. @[0,1,2,...,255] -> FAIL@
instance Pretty Fail where
    pretty = \case
      FailShow wt st mwv detail -> Pretty.vsep $
        case mwv of
          Nothing -> [typeDoc, detailDoc]
          Just wv ->
            let valueDoc = "value: "<+>pretty wv<+>"->"<+>"FAIL"
            in  [typeDoc, valueDoc, detailDoc]
        where
          typeDoc   = "type:  "<+>prettyTypeRep wt<+>"->"<+>prettyTypeRep st
          detailDoc = case detail of
            []           -> "<no detail>"
            [detailLine] -> "detail:"<+>pretty detailLine
            _            -> "detail:"<>Pretty.line<>prettyList detail

      FailOther detail -> pretty detail

      -- TODO should inspect meta, shorten if identical (currently only using
      -- weak)
      FailField dw _ds cw _cs iw fw _is _fs es ->
        let sw = maybe (show iw) id fw
        in  Pretty.nest 0 $ pretty dw<>"."<>pretty cw<>"."<>pretty sw<>Pretty.line<>prettyList es

-- mutually recursive with its 'Pretty' instance. safe, but a bit confusing -
-- clean up
prettyList :: (Foldable f, Pretty a) => f a -> Pretty.Doc ann
prettyList = Pretty.vsep . map go . Foldable.toList
  where go e = "-"<+>Pretty.indent 0 (pretty e)

-- | Succeed on 'Just', fail with given detail on 'Nothing'.
maybeFailShow
    :: forall a. (Typeable (Weak a), Typeable a)
    => [Text] -> Maybe a -> Result a
maybeFailShow detail = \case
    Just a  -> Success a
    Nothing -> failShowNoVal @(Weak a) detail

-- | Assert a predicate to refine a type.
instance (Refine p a, Typeable a, Typeable p, Typeable k)
  => Strengthen (Refined (p :: k) a) where
    strengthen = refine .> \case
      Left  rf -> failShowNoVal @a
        [ "refinement: "<>tshow (typeRep' @p)
        , "failed with..."
        , prettyRefineFailure rf
        ]
      Right ra -> Success ra

-- | Assert a functor predicate to refine a type.
instance (Refine1 p f, Typeable f, Typeable (a :: ak), Typeable ak, Typeable p, Typeable k)
  => Strengthen (Refined1 (p :: k) f a) where
    strengthen = refine1 .> \case
      Left  rf -> failShowNoVal @(f a)
        [ "refinement: "<>tshow (typeRep' @p)
        , "failed with..."
        , prettyRefineFailure rf
        ]
      Right ra -> Success ra

-- | Strengthen a plain list into a non-empty list by asserting non-emptiness.
instance Typeable a => Strengthen (NonEmpty a) where
    strengthen = NonEmpty.nonEmpty .> maybeFailShow ["empty list"]

-- | Strengthen a plain list into a sized vector by asserting length.
instance
  ( VG.Vector v a, KnownNat n
  , Typeable v, Typeable a
  ) => Strengthen (VGS.Vector v n a) where
      strengthen = VGS.fromList .> maybeFailShow ["incorrect length"]

-- | Add wrapper.
instance Strengthen (Identity a) where
    strengthen = pure <$> Identity

-- | Add wrapper.
instance Strengthen (Const a b) where
    strengthen = pure <$> Const

{- TODO controversial. seems logical, but also kinda annoying.
instance (Show a, Typeable a) => Strengthen (Maybe a) where
    strengthen = \case [a] -> pure $ Just a
                       []  -> pure Nothing
                       x   -> strengthenFailBase x "list wasn't [a] or []"
-}

-- Strengthen 'Natural's into Haskell's bounded unsigned numeric types.
instance Strengthen Word8  where strengthen = strengthenBounded
instance Strengthen Word16 where strengthen = strengthenBounded
instance Strengthen Word32 where strengthen = strengthenBounded
instance Strengthen Word64 where strengthen = strengthenBounded

-- Strengthen 'Integer's into Haskell's bounded signed numeric types.
instance Strengthen Int8   where strengthen = strengthenBounded
instance Strengthen Int16  where strengthen = strengthenBounded
instance Strengthen Int32  where strengthen = strengthenBounded
instance Strengthen Int64  where strengthen = strengthenBounded

-- | Strengthen one numeric type into another.
--
-- @n@ must be "wider" than @m@.
strengthenBounded
    :: forall m n
    .  ( Typeable n, Integral n, Show n
       , Typeable m, Integral m, Show m, Bounded m
       ) => n -> Result m
strengthenBounded n
  | n <= maxB && n >= minB = Success (fromIntegral n)
  | otherwise = failShow n
        [ "not well bounded, require: "
          <>tshow minB<>" <= n <= "<>tshow maxB
        ]
  where
    maxB = fromIntegral @m @n maxBound
    minB = fromIntegral @m @n minBound

--------------------------------------------------------------------------------

-- | Decomposer. Strengthen every element in a list.
instance Strengthen a => Strengthen [a] where
    strengthen = traverse strengthen

-- | Decomposer. Strengthen both elements of a tuple.
instance (Strengthen a, Strengthen b) => Strengthen (a, b) where
    strengthen (a, b) = liftA2 (,) (strengthen a) (strengthen b)

-- | Decomposer. Strengthen either side of an 'Either'.
instance (Strengthen a, Strengthen b) => Strengthen (Either a b) where
    strengthen = \case Left  a -> Left  <$> strengthen a
                       Right b -> Right <$> strengthen b

--------------------------------------------------------------------------------

prettyTypeRep :: TypeRep -> Pretty.Doc a
prettyTypeRep = pretty . show

-- from flow
(.>) :: (a -> b) -> (b -> c) -> a -> c
f .> g = g . f
