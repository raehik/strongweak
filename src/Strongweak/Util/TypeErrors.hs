module Strongweak.Util.TypeErrors where

import GHC.TypeError

type ErrZeroInvariantNewtype a =
       Text a :<>: Text " is a zero-invariant, coercible newtype"
  :$$: Text "These may not be used with strongweak type classes directly"
  :$$: Text "  due to ambiguity with their handling"
  :$$: Text "Wrap it using Strongweak.Coercibly"
