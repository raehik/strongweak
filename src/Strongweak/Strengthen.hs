{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}

module Strongweak.Strengthen where

import GHC.TypeNats ( Natural, KnownNat )
import Data.Word
import Data.Int
import Refined ( Refined, refine, Predicate )
import Data.Vector.Sized qualified as Vector
import Data.Vector.Sized ( Vector )
import Type.Reflection ( Typeable, typeRep )

import Prettyprinter
import Prettyprinter.Render.String

import Data.Validation
import Data.List.NonEmpty ( NonEmpty( (:|) ) )
import Data.Foldable qualified as Foldable

{- | Any 'w' can be "strengthened" into an 's' by asserting some properties.

For example, you may strengthen some 'Natural' @n@ into a 'Word8' by asserting
@0 <= n <= 255@.

Note that we restrict strengthened types to having only one corresponding weak
representation using functional dependencies.
-}
class Strengthen w s | s -> w where strengthen :: w -> Validation (NonEmpty StrengthenError) s

-- | 'strengthen' with reordered type variables for more convenient visible type
--   application.
strengthen' :: forall s w. Strengthen w s => w -> Validation (NonEmpty StrengthenError) s
strengthen' = strengthen

data StrengthenError
  = StrengthenErrorBase String String String String
  -- ^ weak type, strong type, weak value, msg

  | StrengthenErrorField String String String String (Either Natural String) (Either Natural String) (NonEmpty StrengthenError)
  -- ^ weak datatype name, strong datatype name,
  --   weak constructor name, strong constructor name,
  --   weak field name, strong field name,
  --   errors
  --
  -- Fields use their record name if present, else their index in the
  -- constructor (from 0).
    deriving stock Eq

instance Show StrengthenError where
    showsPrec _ = renderShowS . layoutPretty defaultLayoutOptions . pretty

-- TODO shorten value if over e.g. 50 chars. e.g. @[0,1,2,...,255] -> FAIL@
instance Pretty StrengthenError where
    pretty = \case
      StrengthenErrorBase wt st wv msg ->
        vsep [ pretty wt<+>"->"<+>pretty st
             , pretty wv<+>"->"<+>"FAIL"
             , pretty msg ]
      StrengthenErrorField dw _ds cw _cs sw _ss es ->
        let sw' = either show id sw
        in  nest 0 $ pretty dw<>"."<>pretty cw<>"."<>pretty sw'<>line<>strengthenErrorPretty es

-- mutually recursive with its 'Pretty' instance. safe, but a bit confusing -
-- clean up
strengthenErrorPretty :: NonEmpty StrengthenError -> Doc a
strengthenErrorPretty = vsep . map go . Foldable.toList
  where go e = "-"<+>indent 0 (pretty e)

strengthenErrorBase
    :: forall s w. (Typeable w, Show w, Typeable s)
    => w -> String -> Validation (NonEmpty StrengthenError) s
strengthenErrorBase w msg = Failure (e :| [])
  where e = StrengthenErrorBase (show $ typeRep @w) (show $ typeRep @s) (show w) msg

-- | Strengthen each element of a list.
instance Strengthen w s => Strengthen [w] [s] where
    strengthen = traverse strengthen

-- | Obtain a sized vector by asserting the size of a plain list.
instance (KnownNat n, Typeable a, Show a) => Strengthen [a] (Vector n a) where
    strengthen w =
        case Vector.fromList w of
          Nothing -> strengthenErrorBase w "TODO bad size vector"
          Just s  -> Success s

-- | Obtain a refined type by applying its associated refinement.
instance (Predicate (p :: k) a, Typeable k, Typeable a, Show a) => Strengthen a (Refined p a) where
    strengthen a =
        case refine a of
          Left  err -> strengthenErrorBase a (show err)
          Right ra  -> Success ra

-- Strengthen 'Natural's into Haskell's bounded unsigned numeric types.
instance Strengthen Natural Word8  where strengthen = strengthenBounded
instance Strengthen Natural Word16 where strengthen = strengthenBounded
instance Strengthen Natural Word32 where strengthen = strengthenBounded
instance Strengthen Natural Word64 where strengthen = strengthenBounded

-- Strengthen 'Integer's into Haskell's bounded signed numeric types.
instance Strengthen Integer Int8   where strengthen = strengthenBounded
instance Strengthen Integer Int16  where strengthen = strengthenBounded
instance Strengthen Integer Int32  where strengthen = strengthenBounded
instance Strengthen Integer Int64  where strengthen = strengthenBounded

strengthenBounded
    :: forall b n
    .  (Integral b, Bounded b, Show b, Typeable b, Integral n, Show n, Typeable n)
    => n -> Validation (NonEmpty StrengthenError) b
strengthenBounded n =
    if   n <= maxB && n >= minB then Success (fromIntegral n)
    else strengthenErrorBase n $ "not well bounded, require: "
                                 <>show minB<>" <= n <= "<>show maxB
  where
    maxB = fromIntegral @b @n maxBound
    minB = fromIntegral @b @n minBound
