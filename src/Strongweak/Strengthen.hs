{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Strongweak.Strengthen
  (
  -- * 'Strengthen' class
    Strengthen(..)
  , restrengthen

  -- ** Helpers
  , strengthenBounded

  -- * Strengthen failures
  , StrengthenFailure(..)
  , StrengthenFailure'
  , failStrengthen1
  , failStrengthen

  -- * Re-exports
  , Strongweak.Weaken.Weakened
  ) where

import Strongweak.Util.TypeNats ( natVal'' )
import Strongweak.Weaken ( Weaken(Weakened, weaken) )

import GHC.TypeNats ( KnownNat )
import Data.Word
import Data.Int
import Rerefined
import Data.Vector.Generic.Sized qualified as VGS -- Shazbot!
import Data.Vector.Generic qualified as VG
import Data.Functor.Identity
import Data.Functor.Const
import Data.List.NonEmpty qualified as NonEmpty
import Data.List.NonEmpty ( NonEmpty )

import Data.Text.Builder.Linear qualified as TBL
import GHC.Exts ( fromString )

import Data.Bits ( FiniteBits )

import Data.Typeable ( Typeable, TypeRep, typeRep, Proxy(Proxy) )

{- | Attempt to strengthen some @'Weakened' a@, asserting certain invariants.

We take 'Weaken' as a superclass in order to maintain strong/weak type pair
consistency. We choose this dependency direction because we treat the strong
type as the "canonical" one, so 'Weaken' is the more natural (and
straightforward) class to define. That does mean the instances for this class
are a little confusingly worded. Alas.

See "Strongweak" for class design notes and laws.
-}
class Weaken a => Strengthen a where
    -- | Attempt to strengthen some @'Weakened' a@ to its associated strong type
    --   @a@.
    strengthen :: Weakened a -> Either StrengthenFailure' a

-- | Weaken a strong value, then strengthen it again.
--
-- Potentially useful if you have previously used
-- 'Strongweak.Strengthen.Unsafe.unsafeStrengthen' and now wish to check the
-- invariants. For example:
--
-- >>> restrengthen $ unsafeStrengthen @(Vector 2 Natural) [0]
-- Left ...
restrengthen :: (Strengthen a, Weaken a) => a -> Either StrengthenFailure' a
restrengthen = strengthen . weaken

-- | A failure encountered during strengthening.
--
-- Strengthening can involve multiple distinct checks. In such cases, you may
-- record multiple failures in a single 'StrengthenFailure' by placing them in
-- the inner failure list and noting their meaning in the detail field.
data StrengthenFailure text = StrengthenFailure
  { strengthenFailDetail :: [text]
  -- ^ Detail on strengthen failure.
  --
  -- We use a list here for the cases where you want multiple lines of detail.
  -- Separating with a newline would make prettifying later harder, so we delay.
  --
  -- Note that this should probably never be empty. TODO consider @NonEmpty@,
  -- but fairly unimportant.

  , strengthenFailInner  :: [(text, StrengthenFailure text)]
  -- ^ Optional wrapped failures.
  --
  -- The @text@ type acts as an index. Its meaning depends on the failure
  -- in question, and should be explained in 'strengthenFailDetail'.
  } deriving stock Show

type StrengthenFailure' = StrengthenFailure TBL.Builder

-- | Shorthand for failing a strengthen.
failStrengthen
    :: [text] -> [(text, StrengthenFailure text)]
    -> Either (StrengthenFailure text) a
failStrengthen t fs = Left $ StrengthenFailure t fs

-- | Shorthand for failing a strengthen with no inner failures.
failStrengthen1 :: [text] -> Either (StrengthenFailure text) a
failStrengthen1 t = failStrengthen t []

-- | Strengthen a type by refining it with a predicate.
instance Refine p a => Strengthen (Refined p a) where
    strengthen = refine .> \case
      Right ra -> Right ra
      Left  rf -> failStrengthen1
        [ "refinement failure:"
        , TBL.fromText (prettyRefineFailure rf) ]
        -- ^ TODO rerefined: provide a TBL pretty function

-- | Strengthen a type by refining it with a functor predicate.
instance Refine1 p f => Strengthen (Refined1 p f a) where
    strengthen = refine1 .> \case
      Right ra -> Right ra
      Left  rf -> failStrengthen1
        [ "refinement failure:"
        , TBL.fromText (prettyRefineFailure rf) ]

-- | Strengthen a plain list into a non-empty list by asserting non-emptiness.
instance Strengthen (NonEmpty a) where
    strengthen = NonEmpty.nonEmpty .> \case
      Just neas -> Right neas
      Nothing   -> failStrengthen1 $
        [ "type: [a] -> NonEmpty a"
        , "fail: empty list" ]

-- | Strengthen a plain list into a sized vector by asserting length.
instance (VG.Vector v a, KnownNat n) => Strengthen (VGS.Vector v n a) where
    -- as of text-linear-builder-0.1.3, we can use 'fromUnboundedDec' for the
    -- phantom vector size
    -- I don't believe you can actually ever construct a vector with size
    -- greater than @'maxBound' \@'Int'@. but still!
    strengthen as =
        case VGS.fromList as of
          Just va -> Right va
          Nothing -> failStrengthen1 $
            [ "type: [a] -> Vector v "<>TBL.fromUnboundedDec n<>" a"
            , "fail: wrong length (got "<>TBL.fromDec (length as)<>")" ]
      where n = natVal'' @n

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
--
-- @'FiniteBits' m@ is for error printing.
strengthenBounded
    :: forall m n
    .  ( Typeable n, Integral n
       , Typeable m, Integral m, Bounded m, FiniteBits m
       ) => n -> Either StrengthenFailure' m
strengthenBounded n
  | n <= maxBn && n >= minBn = Right (fromIntegral n)
  | otherwise = failStrengthen1
        [ "numeric strengthen: "<>fromString (show (typeRep' @n))
          <>" -> "<>fromString (show (typeRep' @m))
        , "bounds check does not hold: "
          <>TBL.fromDec minBm<>" <= "<>TBL.fromUnboundedDec n
          <>" <= "<>TBL.fromDec maxBm
        ]
  where
    maxBn = fromIntegral @m @n maxBm
    minBn = fromIntegral @m @n minBm
    maxBm = maxBound @m
    minBm = minBound @m

--------------------------------------------------------------------------------

-- | Decomposer. Strengthen every element in a list.
instance Strengthen a => Strengthen [a] where
    strengthen = strengthenList

-- TODO using reverse, SLOW!! >:(
strengthenList :: Strengthen a => [Weakened a] -> Either StrengthenFailure' [a]
strengthenList = goR (0 :: Int) [] . map strengthen
  where
    goR i as = \case
      r:rs ->
        case r of
          Right a -> goR (i+1) (a:as) rs
          Left  e -> goL (i+1) [(TBL.fromDec i, e)]    rs
      []   -> Right (reverse as)
    goL i es = \case
      r:rs ->
        case r of
          Right _ -> goL (i+1) es                      rs
          Left  e -> goL (i+1) ((TBL.fromDec i, e):es) rs
      []   -> failStrengthen ["list had failures"] es

-- | Decomposer. Strengthen both elements of a tuple.
instance (Strengthen l, Strengthen r) => Strengthen (l, r) where
    strengthen (l, r) =
        case strengthen l of
          Right sl ->
            case strengthen r of
              Right sr -> Right (sl, sr)
              Left  er -> failStrengthen ["2-tuple: right failed"]
                [("R", er)]
          Left  el ->
            case strengthen @r r of
              Right _  -> failStrengthen ["2-tuple:  left failed"]
                [("L", el)]
              Left  er -> failStrengthen ["2-tuple:   l&r failed"]
                [("R", er), ("L", el)]

-- | Decomposer. Strengthen either side of an 'Either'.
instance (Strengthen a, Strengthen b) => Strengthen (Either a b) where
    strengthen = \case Left  a -> Left  <$> strengthen a
                       Right b -> Right <$> strengthen b

--------------------------------------------------------------------------------

-- from flow
(.>) :: (a -> b) -> (b -> c) -> a -> c
f .> g = g . f

typeRep' :: forall a. Typeable a => TypeRep
typeRep' = typeRep (Proxy @a)

instance Strengthen (Identity a) where
    strengthen = Right . Identity
