module Strongweak.StrengthenSpec ( spec ) where

import Strongweak
import Common
import Data.Either.Validation
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
            e = sfGeneric1
                    "DP" "DP" "DP" "DP" 0 (Just "dp1f0") 0 (Just "dp1f0")
                    "Natural" "Word32" w
        strengthen @(DP 'Strong) d `shouldSatisfy` svEqFail e
    it "returns a precise error for failed generic strengthening (unnamed field)" $ do
        let w = fromIntegral (maxBound @Word8) + 1
            d = DS0 0 1 2 3 w :: DS 'Weak
            e = sfGeneric1
                    "DS" "DS" "DS0" "DS0" 4 Nothing 4 Nothing
                    "Natural" "Word8" w
        strengthen @(DS 'Strong) d `shouldSatisfy` svEqFail e

sfGeneric1
    :: Show w
    => String -> String -> String -> String
    -> Natural -> Maybe String -> Natural -> Maybe String
    -> String -> String -> w
    -> StrengthenFail
sfGeneric1 dw ds cw cs iw fw is fs tw ts w =
    StrengthenFailField dw ds cw cs iw fw is fs (e :| [])
  where e = StrengthenFailBase tw ts (show w) (error "TODO ignoring msg")

sfEqIgnoreMsg :: StrengthenFail -> StrengthenFail -> Bool
sfEqIgnoreMsg s1 s2 = case s1 of
  StrengthenFailField   dw  ds  cw  cs  iw  fw  is  fs  es -> case s2 of
    StrengthenFailField dw' ds' cw' cs' iw' fw' is' fs' es' ->
         dw == dw'
      && ds == ds'
      && cw == cw'
      && cs == cs'
      && iw == iw'
      && fw == fw'
      && is == is'
      && fs == fs'
      && and (zipWith sfEqIgnoreMsg (Foldable.toList es) (Foldable.toList es'))
    _ -> False
  StrengthenFailBase   wt  st  wv  _ -> case s2 of
    StrengthenFailBase wt' st' wv' _ ->
         wt == wt'
      && st == st'
      && wv == wv'
    _ -> False

svEqFail :: StrengthenFail -> Validation (NonEmpty StrengthenFail) s -> Bool
svEqFail e = \case Failure (e' :| []) -> sfEqIgnoreMsg e e'
                   _ -> False
