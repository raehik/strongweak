module Strongweak.StrengthenSpec where -- ( spec ) where

import Strongweak.Util.Typeable
import Strongweak.Util.Text
import Strongweak
import Strongweak.Strengthen
import Common
import Data.Either.Validation
import Test.Hspec

import Numeric.Natural ( Natural )
import Data.Word
import Data.Foldable qualified as Foldable
import Data.Typeable ( TypeRep )

spec :: Spec
spec = undefined

{-

spec :: Spec
spec = do
    it "returns a precise error for failed generic strengthening (named field)" $ do
        let w = fromIntegral (maxBound @Word32) + 1
            d = DP w 43 1 2 3 :: DP 'Weak
            e = sfGenericSW1Show
                    "DP.DP" "0.dp1f0"
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
    -> Fail
sfGenericSW1Show d c i f tw ts w =
    FailField d d c c i f i f (pure e)
  where
    e = FailShow tw ts (Just (tshow w)) detail
    detail = error "tried to check failure descriptions in tests (bad idea)"

-- only test field and show, and ignore message in latter
sfEq :: Fail -> Fail -> Bool
sfEq s1 s2 = case s1 of
  FailField   dw  ds  cw  cs  iw  fw  is  fs  es -> case s2 of
    FailField dw' ds' cw' cs' iw' fw' is' fs' es' ->
         dw == dw' && ds == ds'
      && cw == cw' && cs == cs'
      && iw == iw' && is == is'
      && fw == fw' && fs == fs'
      && and (zipWith sfEq (Foldable.toList es) (Foldable.toList es'))
    _ -> False
  FailShow   wt  st  wv  _ -> case s2 of
    FailShow wt' st' wv' _ ->
         wt  == wt' && st == st'
      && wv  == wv'
    _ -> False
  _ -> error "unexpected strengthen fail"

svEqFail :: Fail -> Result s -> Bool
svEqFail e = \case
  Success{}  -> False
  Failure es ->
    case Foldable.toList es of
      [e'] -> sfEq e e'
      _    -> False

-}
