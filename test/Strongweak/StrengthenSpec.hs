module Strongweak.StrengthenSpec ( spec ) where

import Strongweak
import Common
import Data.Validation
import Test.Hspec

import Numeric.Natural ( Natural )
import Data.Word
import Data.List.NonEmpty ( NonEmpty(..) )
import Data.Foldable qualified as Foldable

spec :: Spec
spec = do
    it "returns a precise error for failed generic strengthening (named field)" $ do
        let w = fromIntegral (maxBound @Word32) + 1
            d = DP w 43 1 2 3 :: DP 'Weak
            e = seGeneric1
                    "DP" "DP" "DP" "DP" 0 (Just "dp1f0") 0 (Just "dp1f0")
                    "Natural" "Word32" w
        strengthen @_ @(DP 'Strong) d `shouldSatisfy` svEqError e
    it "returns a precise error for failed generic strengthening (unnamed field)" $ do
        let w = fromIntegral (maxBound @Word8) + 1
            d = DS0 0 1 2 3 w :: DS 'Weak
            e = seGeneric1
                    "DS" "DS" "DS0" "DS0" 4 Nothing 4 Nothing
                    "Natural" "Word8" w
        strengthen @_ @(DS 'Strong) d `shouldSatisfy` svEqError e

seGeneric1
    :: Show w
    => String -> String -> String -> String
    -> Natural -> Maybe String -> Natural -> Maybe String
    -> String -> String -> w
    -> StrengthenError
seGeneric1 dw ds cw cs iw fw is fs tw ts w =
    StrengthenErrorField dw ds cw cs iw fw is fs (e :| [])
  where e = StrengthenErrorBase tw ts (show w) (error "TODO ignoring msg")

seEqIgnoreMsg :: StrengthenError -> StrengthenError -> Bool
seEqIgnoreMsg s1 s2 = case s1 of
  StrengthenErrorField   dw  ds  cw  cs  iw  fw  is  fs  es -> case s2 of
    StrengthenErrorField dw' ds' cw' cs' iw' fw' is' fs' es' ->
         dw == dw'
      && ds == ds'
      && cw == cw'
      && cs == cs'
      && iw == iw'
      && fw == fw'
      && is == is'
      && fs == fs'
      && and (zipWith seEqIgnoreMsg (Foldable.toList es) (Foldable.toList es'))
    _ -> False
  StrengthenErrorBase   wt  st  wv  _ -> case s2 of
    StrengthenErrorBase wt' st' wv' _ ->
         wt == wt'
      && st == st'
      && wv == wv'
    _ -> False

svEqError :: StrengthenError -> Validation (NonEmpty StrengthenError) s -> Bool
svEqError e = \case Failure (e' :| []) -> seEqIgnoreMsg e e'
                    _ -> False
