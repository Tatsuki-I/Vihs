module InsertSpec (spec) where

import           Insert
import           Test.Hspec

spec :: Spec
spec = do
        describe "insert" $ do
                it "standard" $ do
                        insert "aaa" "b" 2 `shouldBe` "aaba"
        describe "insertBuff" $ do
                it "standard" $ do
                        insertBuff ["AAA", "BBB", "CCC"] "b" 1 2 `shouldBe` ["AAA", "BBB", "CbCC"]
