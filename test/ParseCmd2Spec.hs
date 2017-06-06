module ParseCmd2Spec (spec) where

import Test.Hspec
import ParseCmd2

spec :: Spec
spec = do
        describe "setCmd" $ do
                it "standard" $ do
                        setCmd "w test.txt" `shouldBe` Command {addr = Nothing, cmdName = 'w', param = Just "test.txt"}
        describe "setCmd" $ do
                it "standard" $ do
                        setCmd "1,2a" `shouldBe` Command {addr = Just (AddrPair (AddrLine 1) (AddrLine 2)), cmdName = 'a', param = Nothing}
        describe "setCmd" $ do
                it "standard" $ do
                        setCmd ",d" `shouldBe` Command {addr = Just (AddrPair (AddrLine 1) AddrEOF), cmdName = 'd', param = Nothing}
