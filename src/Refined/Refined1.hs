{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Refined.Refined1 where

import Refined
import Data.Proxy

-- refinement on functor, not on a
newtype Refined1 (p :: k) f a = Refined1 { unRefined1 :: f a }
    deriving (Functor, Foldable) via f
    deriving stock Traversable

-- TODO what do for this?
--type role Refined nominal nominal

unrefine1 :: Refined1 p f a -> f a
unrefine1 = unRefined1

unsafeRefine1 :: f a -> Refined1 p f a
unsafeRefine1 = Refined1

class Pred p => ApplyPred1 p f where
    validate1 :: Proxy p -> f a -> Maybe RefineException

refine1
    :: forall p f a. ApplyPred1 p f
    => f a -> Either RefineException (Refined1 p f a)
refine1 fa =
    case validate1 (Proxy @p) fa of
      Nothing  -> Right $ Refined1 fa
      Just err -> Left err
