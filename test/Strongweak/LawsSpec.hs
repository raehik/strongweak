module Strongweak.LawsSpec ( spec ) where

import Strongweak
import Common
import Data.Validation
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = modifyMaxSize (+1000) $ do
    prop "weaken-strengthen roundtrip isomorphism (generic)" $ do
      \(d :: DS 'Strong) -> strengthen @(DS 'Weak) (weaken d) `shouldBe` Success d
    prop "strengthen-weaken-strengthen roundtrip partial isomorphism (generic)" $ do
      \(dw :: DS 'Weak) ->
        case strengthen dw of
          Failure _ -> pure ()
          Success (ds :: DS 'Strong) ->
            strengthen @(DS 'Weak) (weaken ds) `shouldBe` Success ds
