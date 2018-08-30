{-# LANGUAGE OverloadedStrings #-}

module Constretto.TypesSpec
  ( typesSpecs
  ) where

import           Constretto.Internal.Types

import           Data.Map.Strict
import qualified Data.Text        as T
import           Test.Hspec

testConstretto :: Constretto
testConstretto =
  Constretto
    (fromList
       [ ("default", fromList [("mykey", "defaultval")])
       , ("dev", fromList [("dbaddress", "dev.db.org"), ("mykey", "devkey")])
       , ("prod", fromList [("dbaddress", "prod.db.org"), ("mykey", "prodkey")])
       ])

typesSpecs :: IO ()
typesSpecs =
  hspec $
  describe "Constretto.Internal.Types" $ do
    it "does a single lookup" $ do
      let res = lookup' "default" "mykey" testConstretto
      res `shouldBe` (Just "defaultval")
    it "falls back to default value in a constretto file" $
      lookup'' [] "mykey" testConstretto `shouldBe` (Just "defaultval")
    it "falls back to default value for tags not mentioned in file" $
      lookup'' ["cat", "fish", "dog"] "mykey" testConstretto `shouldBe`
      (Just "defaultval")
    it "overrides default value when specified" $
      lookup'' ["dev"] "dbaddress" testConstretto `shouldBe` (Just "dev.db.org")
    it "overrides in a priority list" $
      lookup'' ["prod", "dev"] "dbaddress" testConstretto `shouldBe`
      (Just "prod.db.org")
