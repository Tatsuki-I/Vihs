module InsertSpec (spec) where

import           Insert
import           Test.Hspec

spec :: Spec
spec =
        describe "insert" $ do
                it "standard" $
                        insert "aaa" "b" 2 `shouldBe` "aaba"
                it "standard" $
                        insertBuff ["AAA", "BBB", "CCC"] "b" 1 2 `shouldBe` ["AAA", "BBB", "CbCC"]
