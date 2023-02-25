{-# LANGUAGE AllowAmbiguousTypes #-}

module Strongweak.Util.Typeable where

import Data.Typeable

typeRep' :: forall a. Typeable a => TypeRep
typeRep' = typeRep (Proxy @a)
