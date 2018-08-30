{-# LANGUAGE OverloadedStrings #-}

module ConstrettoSpec
  ( constrettoSpec
  ) where

import           Constretto
import           Constretto.Internal.Types (emptyConstretto)
import           Data.Either
import           Data.Text                 as T
import           System.Directory
import           Test.Hspec

constrettoSpec :: IO ()
constrettoSpec =
  hspec $
  describe "Constretto" $
  it "parses a constretto file" $ do
    cd <- getCurrentDirectory
    eitherSC <- readConfig $ cd ++ "/test/testconstretto.ini"
    eitherSC `shouldSatisfy` isRight
    let c = either (const emptyConstretto) id eitherSC
    lookupC ["dev"] "dbaddress" c `shouldBe` (Just "dev.db.org")
    lookupC ["prod"] "dbaddress" c `shouldBe` (Just "prod.db.org")
    lookupC ["dev"] "mykey" c `shouldBe` (Just "devkey")
    lookupC ["prod"] "mykey" c `shouldBe` (Just "prodkey")
