module Strongweak.StrengthenSpec ( spec ) where

import Util.Typeable
import Strongweak
import Common
import Data.Either.Validation
import Test.Hspec

import Numeric.Natural ( Natural )
import Data.Word
import Data.List.NonEmpty ( NonEmpty(..) )
import Data.Foldable qualified as Foldable
import Data.Typeable

spec :: Spec
spec = do
    it "returns a precise error for failed generic strengthening (named field)" $ do
        let w = fromIntegral (maxBound @Word32) + 1
            d = DP w 43 1 2 3 :: DP 'Weak
            e = sfGenericSW1Show
                    "DP" "DP" 0 (Just "dp1f0")
                    (typeRep' @Natural) (typeRep' @Word32) w
        strengthen @(DP 'Strong) d `shouldSatisfy` svEqFail e
    it "returns a precise error for failed generic strengthening (unnamed field)" $ do
        let w = fromIntegral (maxBound @Word8) + 1
            d = DS0 0 1 2 3 w :: DS 'Weak
            e = sfGenericSW1Show
                    "DS" "DS0" 4 Nothing
                    (typeRep' @Natural) (typeRep' @Word8) w
        strengthen @(DS 'Strong) d `shouldSatisfy` svEqFail e

-- build strengthen failure
-- one failure, generic with SW, one wrapped failure (detailed)
sfGenericSW1Show
    :: Show w
    => String -> String -> Natural -> Maybe String
    -> TypeRep -> TypeRep -> w
    -> StrengthenFail
sfGenericSW1Show d c i f tw ts w =
    StrengthenFailField d d c c i f i f (e :| [])
  where
    e = StrengthenFailShow tw ts (show w) msg
    msg = error "tried to check failure descriptions in tests (bad idea)"

-- only test field and show, and ignore message in latter
sfEq :: StrengthenFail -> StrengthenFail -> Bool
sfEq s1 s2 = case s1 of
  StrengthenFailField   dw  ds  cw  cs  iw  fw  is  fs  es -> case s2 of
    StrengthenFailField dw' ds' cw' cs' iw' fw' is' fs' es' ->
         dw == dw' && ds == ds'
      && cw == cw' && cs == cs'
      && iw == iw' && is == is'
      && fw == fw' && fs == fs'
      && and (zipWith sfEq (Foldable.toList es) (Foldable.toList es'))
    _ -> False
  StrengthenFailShow   wt  st  wv  _ -> case s2 of
    StrengthenFailShow wt' st' wv' _ ->
         wt  == wt' && st == st'
      && wv  == wv'
    _ -> False
  _ -> error "unexpected strengthen fail"

svEqFail :: StrengthenFail -> Validation (NonEmpty StrengthenFail) s -> Bool
svEqFail e = \case Failure (e' :| []) -> sfEq e e'
                   _ -> False
