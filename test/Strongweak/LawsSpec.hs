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
        case strengthen (weaken d) of
          Failure _  -> expectationFailure "roundtrip fail"
          Success ds -> ds `shouldBe` d
    prop "strengthen-weaken-strengthen roundtrip partial isomorphism (generic)" $ do
      \(dw :: DS 'Weak) ->
        case strengthen @(DS 'Strong) dw of
          Failure _  -> pure ()
          Success ds ->
            case strengthen (weaken ds) of
              Failure _   -> expectationFailure "roundtrip fail"
              Success ds' -> ds `shouldBe` ds'
