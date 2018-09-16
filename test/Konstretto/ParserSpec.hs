{-# LANGUAGE OverloadedStrings #-}
module Konstretto.ParserSpec(parserSpecs) where

import           Konstretto.Parser
import           Konstretto.Internal.Types

import qualified Data.Attoparsec.Text as AT
import           Data.Either
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import           System.Directory
import           Test.Hspec

parserSpecs :: IO ()
parserSpecs =
  hspec $
  describe "KonstrettoParser" $
    it "parses a konstretto file" $ do
      cd <- getCurrentDirectory
      t <- TIO.readFile $ cd ++ "/test/testkonstretto.ini"
      let r = AT.parseOnly konstrettoParser t
      print r
      r `shouldSatisfy` isRight
      let c = fromRight emptyKonstretto r
      lookup' "dev" "dbaddress" c `shouldBe` (Just "dev.db.org")
      lookup' "prod" "dbaddress" c `shouldBe` (Just "prod.db.org")
      lookup' "dev" "mykey" c `shouldBe` (Just "devkey")
      lookup' "prod" "mykey" c `shouldBe` (Just "prodkey")
