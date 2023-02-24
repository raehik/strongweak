{-# LANGUAGE OverloadedStrings #-}

module Strongweak.Strengthen
  (
  -- * 'Strengthen' class
    Strengthen(..)

  -- * Strengthen failures
  , StrengthenFail(..)
  , TryStrengthen
  , strengthenFailPretty
  , strengthenFailShow

  -- * Restrengthening
  , restrengthen

  -- * Helpers
  , strengthenBounded

  -- * Re-exports
  , Strongweak.Weaken.Weak
  ) where


import Util.Typeable ( typeRep' )
import Strongweak.Weaken ( Weaken(..) )
import Data.Either.Validation
import Data.Typeable ( Typeable, TypeRep )
import Prettyprinter
import Prettyprinter.Render.String

import GHC.TypeNats ( Natural, KnownNat )
import Data.Word
import Data.Int
import Refined hiding ( Weaken, weaken, strengthen, NonEmpty )
import Data.Vector.Generic.Sized qualified as VGS -- Shazbot!
import Data.Vector.Generic qualified as VG
import Data.Foldable qualified as Foldable
import Control.Applicative ( liftA2 )
import Data.Functor.Identity
import Data.Functor.Const
import Data.List.NonEmpty ( NonEmpty( (:|) ) )
import Data.List.NonEmpty qualified as NonEmpty

type TryStrengthen = Validation (NonEmpty StrengthenFail)

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
    strengthen :: Weak a -> TryStrengthen a

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
    => a -> TryStrengthen a
restrengthen = strengthen . weaken

-- | A failure encountered during strengthening.
data StrengthenFail
  -- | A refinement failure.
  = StrengthenFailRefine
        TypeRep         -- ^ predicate name
        RefineException -- ^ refine error

  -- | Some failures occurred when strengthening from one data type to another.
  --
  -- Field indices are from 0 in the respective constructor. Field names are
  -- provided if are present in the type.
  --
  -- This is primarily intended to be used by generic strengtheners.
  | StrengthenFailField
        String                      -- ^ weak   datatype name
        String                      -- ^ strong datatype name
        String                      -- ^ weak   constructor name
        String                      -- ^ strong constructor name
        Natural                     -- ^ weak   field index
        (Maybe String)              -- ^ weak   field name (if present)
        Natural                     -- ^ strong field index
        (Maybe String)              -- ^ strong field name (if present)
        (NonEmpty StrengthenFail)   -- ^ failures

  -- | A failure containing lots of detail. Use in concrete instances where you
  --   already have the 'Show's and 'Typeable's needed.
  | StrengthenFailShow
        TypeRep -- ^ weak   type
        TypeRep -- ^ strong type
        String -- ^ weak value
        String -- ^ failure description

  -- | A failure. Use in abstract instances to avoid heavy contexts. (Remember
  --   that generic strengtheners should wrap these nicely anyway!)
  | StrengthenFailOther
        String -- ^ failure description

instance Show StrengthenFail where
    showsPrec _ = renderShowS . layoutPretty defaultLayoutOptions . pretty

-- TODO shorten value if over e.g. 50 chars. e.g. @[0,1,2,...,255] -> FAIL@
instance Pretty StrengthenFail where
    pretty = \case
      -- TODO give RefineException a nice pretty instance... some helpers should
      -- be left in the library too
      StrengthenFailRefine p rex -> vsep
        [ "refinement: "<+>prettyTypeRep p
        , "failed with: "<+>pretty (displayRefineException rex)
        ]
      StrengthenFailOther msg -> vsep
        [ pretty msg
        ]
      -- TODO we have a lot k
      StrengthenFailField dw _ds cw _cs iw fw _is _fs es ->
        let sw = maybe (show iw) id fw
        in  nest 0 $ pretty dw<>"."<>pretty cw<>"."<>pretty sw<>line<>strengthenFailPretty es
      StrengthenFailShow wt st wv msg -> vsep
        [ prettyTypeRep wt<+>"->"<+>prettyTypeRep st
        , pretty wv<+>"->"<+>"FAIL"
        , pretty msg
        ]

prettyTypeRep :: TypeRep -> Doc a
prettyTypeRep = pretty . show

-- mutually recursive with its 'Pretty' instance. safe, but a bit confusing -
-- clean up
strengthenFailPretty :: NonEmpty StrengthenFail -> Doc a
strengthenFailPretty = vsep . map go . Foldable.toList
  where go e = "-"<+>indent 0 (pretty e)

strengthenFailShow
    :: forall s w. (Typeable w, Show w, Typeable s)
    => w -> String -> TryStrengthen s
strengthenFailShow w msg = strengthenFail $ StrengthenFailShow
    (typeRep' @w) (typeRep' @s) (show w) msg

-- one fail please
strengthenFail :: StrengthenFail -> TryStrengthen a
strengthenFail e = Failure (e :| [])

-- | Assert a predicate to refine a type.
instance Predicate p a => Strengthen (Refined p a) where
    strengthen = refine .> \case
      Left  rex -> strengthenFail $ StrengthenFailRefine (typeRep' @p) rex
      Right ra  -> Success ra

-- from flow
(.>) :: (a -> b) -> (b -> c) -> a -> c
f .> g = g . f

-- | Strengthen a plain list into a non-empty list by asserting non-emptiness.
instance Strengthen (NonEmpty a) where
    strengthen = NonEmpty.nonEmpty .> maybeToTryStrengthen "empty list"

maybeToTryStrengthen :: String -> Maybe a -> TryStrengthen a
maybeToTryStrengthen err = \case
    Just a  -> Success a
    Nothing -> strengthenFail $ StrengthenFailOther err

-- | Strengthen a plain list into a sized vector by asserting length.
instance (VG.Vector v a, KnownNat n) => Strengthen (VGS.Vector v n a) where
      strengthen = VGS.fromList .> maybeToTryStrengthen "TODO bad size vector"

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
       ) => n -> TryStrengthen m
strengthenBounded n
  | n <= maxB && n >= minB = Success (fromIntegral n)
  | otherwise = strengthenFailShow n $
          "not well bounded, require: "
        <>show minB<>" <= n <= "<>show maxB
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
