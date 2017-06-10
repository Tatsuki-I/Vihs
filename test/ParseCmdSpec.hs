module ParseCmdSpec (spec) where

import           ParseCmd
import           Test.Hspec

spec :: Spec
spec =
        describe "setCmd" $ do
                it "standard" $
                        setCmd "w test.txt" `shouldBe` Command {addr = Nothing, cmdName = 'w', param = Just "test.txt"}
                it "standard" $
                        setCmd "1,2a" `shouldBe` Command {addr = Just (AddrPair (AddrLine 1) (AddrLine 2)), cmdName = 'a', param = Nothing}
                it "standard" $
                        setCmd ",d" `shouldBe` Command {addr = Just (AddrPair (AddrLine 1) AddrEOF), cmdName = 'd', param = Nothing}
