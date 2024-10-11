module Strongweak.LawsSpec ( spec ) where

import Strongweak
import Common
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = modifyMaxSize (+1000) $ do
    prop "weaken-strengthen roundtrip isomorphism (generic)" $ do
      \(d :: DS Strong) ->
        strengthen (weaken d) `shouldSatisfy` tryStrengthenSuccessEq d
    prop "strengthen-weaken-strengthen roundtrip partial isomorphism (generic)" $ do
      \(dw :: DS Weak) ->
        case strengthen @(DS Strong) dw of
          Right ds ->
            strengthen (weaken ds) `shouldSatisfy` tryStrengthenSuccessEq ds
          Left{}  -> pure ()
