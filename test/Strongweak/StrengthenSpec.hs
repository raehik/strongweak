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
            d = DP { dp1f1 = w, dp1f2 = 43  }
            e = seGeneric1
                    "DP" "DP" "DP" "DP" (Right "dp1f1") (Right "dp1f1")
                    "Natural" "Word32" w
        strengthen' @(DP 'Strong) d `shouldSatisfy` svEqError e
    it "returns a precise error for failed generic strengthening (unnamed field)" $ do
        let w = fromIntegral (maxBound @Word8) + 1
            d = DS1 w
            e = seGeneric1
                    "DS" "DS" "DS1" "DS1" (Left 0) (Left 0)
                    "Natural" "Word8" w
        strengthen' @(DS 'Strong) d `shouldSatisfy` svEqError e

seGeneric1
    :: Show w
    => String -> String -> String -> String -> Either Natural String -> Either Natural String
    -> String -> String -> w
    -> StrengthenError
seGeneric1 dw ds cw cs sw ss tw ts w =
    StrengthenErrorField dw ds cw cs sw ss (e :| [])
  where e = StrengthenErrorBase tw ts (show w) (error "TODO ignoring msg")

seEqIgnoreMsg :: StrengthenError -> StrengthenError -> Bool
seEqIgnoreMsg s1 s2 = case s1 of
  StrengthenErrorField   fw  fs  cw  cs  sw  ss  es -> case s2 of
    StrengthenErrorField fw' fs' cw' cs' sw' ss' es' ->
         fw == fw'
      && fs == fs'
      && cw == cw'
      && cs == cs'
      && sw == sw'
      && ss == ss'
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
