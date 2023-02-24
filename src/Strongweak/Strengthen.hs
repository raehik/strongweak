{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Strongweak.Strengthen
  (
  -- * 'Strengthen' class
    Strengthen(..)

  -- * Strengthen failures
  , StrengthenFail(..)
  , strengthenFailPretty
  , strengthenFailBase

  -- * Restrengthening
  , restrengthen

  -- * Helpers
  , strengthenBounded

  -- * Re-exports
  , Strongweak.Weaken.Weak
  ) where

import Strongweak.Weaken ( Weaken(..) )
import Data.Either.Validation
import Type.Reflection ( Typeable, typeRep )
import Prettyprinter
import Prettyprinter.Render.String

import GHC.TypeNats ( Natural, KnownNat )
import Data.Word
import Data.Int
import Refined ( Refined, refine, ApplyPred, Pred, predName, displayRefineException, RefineException )
import Refined.Refined1
import Data.Vector.Generic.Sized qualified as VGS -- Shazbot!
import Data.Vector.Generic qualified as VG
import Data.Foldable qualified as Foldable
import Control.Applicative ( liftA2 )
import Data.Functor.Identity
import Data.Functor.Const
import Data.List.NonEmpty ( NonEmpty( (:|) ) )
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text ( Text )

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
    strengthen :: Weak a -> Validation (NonEmpty StrengthenFail) a

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
    => a -> Validation (NonEmpty StrengthenFail) a
restrengthen = strengthen . weaken

-- | Strengthen failure data type. Don't use these constructors directly, use
--   the existing helper functions.
--
-- Field indices are from 0 in the respective constructor. Field names are
-- provided if present.
data StrengthenFail
  = StrengthenFailBase
        String -- ^ weak   type
        String -- ^ strong type
        String -- ^ weak value
        String -- ^ msg

  | StrengthenFailRefine
        Text -- ^ predicate name
        RefineException -- ^ refinement error

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
    -- deriving stock Eq

instance Show StrengthenFail where
    showsPrec _ = renderShowS . layoutPretty defaultLayoutOptions . pretty

-- TODO shorten value if over e.g. 50 chars. e.g. @[0,1,2,...,255] -> FAIL@
instance Pretty StrengthenFail where
    pretty = \case
      StrengthenFailRefine p err ->
        vsep [ "refinement: "<+>pretty p
             , "failed with: "<+>pretty (displayRefineException err) ]
      StrengthenFailBase wt st wv msg ->
        vsep [ pretty wt<+>"->"<+>pretty st
             , pretty wv<+>"->"<+>"FAIL"
             , pretty msg ]
      StrengthenFailField dw _ds cw _cs iw fw _is _fs es ->
        let sw = maybe (show iw) id fw
        in  nest 0 $ pretty dw<>"."<>pretty cw<>"."<>pretty sw<>line<>strengthenFailPretty es

-- mutually recursive with its 'Pretty' instance. safe, but a bit confusing -
-- clean up
strengthenFailPretty :: NonEmpty StrengthenFail -> Doc a
strengthenFailPretty = vsep . map go . Foldable.toList
  where go e = "-"<+>indent 0 (pretty e)

strengthenFailBase
    :: forall s w. (Typeable w, Show w, Typeable s)
    => w -> String -> Validation (NonEmpty StrengthenFail) s
strengthenFailBase w msg = Failure (e :| [])
  where e = StrengthenFailBase (show $ typeRep @w) (show $ typeRep @s) (show w) msg

strengthenFailRefine
    :: forall p w. Pred p
    => RefineException -> Validation (NonEmpty StrengthenFail) (Refined p w)
strengthenFailRefine err = Failure (e :| [])
  where e = StrengthenFailRefine (predName @p) err

strengthenFailRefine1
    :: forall p f w. Pred p
    => RefineException -> Validation (NonEmpty StrengthenFail) (Refined1 p f w)
strengthenFailRefine1 err = Failure (e :| [])
  where e = StrengthenFailRefine (predName @p) err

-- | Assert a predicate to refine a type.
instance ApplyPred p a => Strengthen (Refined p a) where
    strengthen a =
        case refine a of
          Left  err -> strengthenFailRefine @p err
          Right ra  -> Success ra

-- | Assert a predicate to refine a functor type.
instance ApplyPred1 p f => Strengthen (Refined1 p f a) where
    strengthen fa =
        case refine1 fa of
          Left  err -> strengthenFailRefine1 @p err
          Right rfa -> Success rfa

-- | Strengthen a plain list into a non-empty list by asserting non-emptiness.
instance (Typeable a, Show a) => Strengthen (NonEmpty a) where
    strengthen a =
        case NonEmpty.nonEmpty a of
          Just a' -> Success a'
          Nothing -> strengthenFailBase a "empty list"

-- | Strengthen a plain list into a sized vector by asserting length.
instance (VG.Vector v a, KnownNat n, Typeable v, Typeable a, Show a)
  => Strengthen (VGS.Vector v n a) where
    strengthen w =
        case VGS.fromList w of
          Nothing -> strengthenFailBase w "TODO bad size vector"
          Just s  -> Success s

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

strengthenBounded
    :: forall b n
    .  (Integral b, Bounded b, Show b, Typeable b, Integral n, Show n, Typeable n)
    => n -> Validation (NonEmpty StrengthenFail) b
strengthenBounded n =
    if   n <= maxB && n >= minB then Success (fromIntegral n)
    else strengthenFailBase n $ "not well bounded, require: "
                                 <>show minB<>" <= n <= "<>show maxB
  where
    maxB = fromIntegral @b @n maxBound
    minB = fromIntegral @b @n minBound

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
