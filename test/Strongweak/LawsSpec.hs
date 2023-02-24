module Strongweak.LawsSpec ( spec ) where

import Strongweak
import Common
import Data.Either.Validation
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = modifyMaxSize (+1000) $ do
    prop "weaken-strengthen roundtrip isomorphism (generic)" $ do
      \(d :: DS 'Strong) ->
        strengthen (weaken d) `shouldSatisfy` tryStrengthenSuccessEq d
    prop "strengthen-weaken-strengthen roundtrip partial isomorphism (generic)" $ do
      \(dw :: DS 'Weak) ->
        case strengthen @(DS 'Strong) dw of
          Failure{}  -> pure ()
          Success ds ->
            strengthen (weaken ds) `shouldSatisfy` tryStrengthenSuccessEq ds
