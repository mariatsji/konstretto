{-# LANGUAGE OverloadedStrings #-}
module Constretto.ParserSpec(parserSpecs) where

import           Constretto.Parser
import           Constretto.Internal.Types

import qualified Data.Attoparsec.Text as AT
import           Data.Either
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import           System.Directory
import           Test.Hspec

parserSpecs :: IO ()
parserSpecs =
  hspec $
  describe "ConstrettoParser" $
    it "parses a constretto file" $ do
      cd <- getCurrentDirectory
      t <- TIO.readFile $ cd ++ "/test/testconstretto.ini"
      let r = AT.parseOnly constrettoParser t
      print r
      r `shouldSatisfy` isRight
      let c = fromRight emptyConstretto r
      lookup' "dev" "dbaddress" c `shouldBe` (Just "dev.db.org")
      lookup' "prod" "dbaddress" c `shouldBe` (Just "prod.db.org")
      lookup' "dev" "mykey" c `shouldBe` (Just "devkey")
      lookup' "prod" "mykey" c `shouldBe` (Just "prodkey")
