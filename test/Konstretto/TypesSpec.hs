{-# LANGUAGE OverloadedStrings #-}

module Konstretto.TypesSpec
  ( typesSpecs
  ) where

import           Konstretto.Internal.Types

import           Data.Map.Strict
import qualified Data.Text        as T
import           Test.Hspec

testKonstretto :: Konstretto
testKonstretto =
  Konstretto
    (fromList
       [ ("default", fromList [("mykey", "defaultval")])
       , ("dev", fromList [("dbaddress", "dev.db.org"), ("mykey", "devkey")])
       , ("prod", fromList [("dbaddress", "prod.db.org"), ("mykey", "prodkey")])
       ])

typesSpecs :: IO ()
typesSpecs =
  hspec $
  describe "Konstretto.Internal.Types" $ do
    it "does a single lookup" $ do
      let res = lookup' "default" "mykey" testKonstretto
      res `shouldBe` (Just "defaultval")
    it "falls back to default value in a konstretto file" $
      lookup'' [] "mykey" testKonstretto `shouldBe` (Just "defaultval")
    it "falls back to default value for tags not mentioned in file" $
      lookup'' ["cat", "fish", "dog"] "mykey" testKonstretto `shouldBe`
      (Just "defaultval")
    it "overrides default value when specified" $
      lookup'' ["dev"] "dbaddress" testKonstretto `shouldBe` (Just "dev.db.org")
    it "overrides in a priority list" $
      lookup'' ["prod", "dev"] "dbaddress" testKonstretto `shouldBe`
      (Just "prod.db.org")
