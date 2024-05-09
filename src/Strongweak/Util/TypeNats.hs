{-# LANGUAGE AllowAmbiguousTypes #-}

module Strongweak.Util.TypeNats where

import GHC.TypeNats
import GHC.Exts ( proxy# )

natVal'' :: forall n. KnownNat n => Natural
natVal'' = natVal' (proxy# @n)
{-# INLINE natVal'' #-}
