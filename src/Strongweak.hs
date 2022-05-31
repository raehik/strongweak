{-# LANGUAGE AllowAmbiguousTypes #-}

module Strongweak
  ( module Strongweak.Weaken
  , module Strongweak.Strengthen, restrengthen
  , module Strongweak.SW
  ) where

import Strongweak.Weaken
import Strongweak.Strengthen
import Strongweak.SW

import Data.Either.Validation
import Data.List.NonEmpty

-- | Weaken and re-strengthen a strong value.
--
-- In correct operation, @restrengthen === Right@. If your value was
-- strengthened incorrectly, or perhaps you cheated via @UnsafeStrengthen@, this
-- may not be the case. For example:
--
-- >>> restrengthen $ unsafeStrengthen' @(Vector 2 Natural) [0]
-- Failure ...
restrengthen
    :: forall w s. (Weaken s w, Strengthen w s)
    => s -> Validation (NonEmpty StrengthenError) s
restrengthen = strengthen . weaken
