{-# LANGUAGE OverloadedStrings #-}

module KonstrettoSpec
  ( konstrettoSpec
  ) where

import           Konstretto
import           Konstretto.Internal.Types (emptyKonstretto)
import           Data.Either
import           Data.Text                 as T
import           System.Directory
import           System.Environment (setEnv)
import           Test.Hspec

konstrettoSpec :: IO ()
konstrettoSpec =
  hspec $
  describe "Konstretto" $ do
  it "parses a konstretto file" $ do
    cd <- getCurrentDirectory
    eitherSC <- readConfig $ cd ++ "/test/testkonstretto.ini"
    eitherSC `shouldSatisfy` isRight
    let c = either (const emptyKonstretto) id eitherSC
    lookupC ["dev"] "dbaddress" c `shouldBe` (Just "dev.db.org")
    lookupC ["prod"] "dbaddress" c `shouldBe` (Just "prod.db.org")
    lookupC ["dev"] "mykey" c `shouldBe` (Just "devkey")
    lookupC ["prod"] "mykey" c `shouldBe` (Just "prodkey")
  it "uses missing CONSTRETTO_TAGS runtime property to get the default key" $ do
    cd <- getCurrentDirectory
    let fp = cd ++ "/test/testkonstretto.ini"
    e <- lookupFromEnvAndFile "CONSTRETTO_TAGS" fp "mykey"
    e `shouldBe` (Right "defaultval")
  it "uses CONSTRETTO_TAGS dev runtime property to get the right key" $ do
    setEnv "CONSTRETTO_TAGS" "dev"
    cd <- getCurrentDirectory
    let fp = cd ++ "/test/testkonstretto.ini"
    e <- lookupFromEnvAndFile "CONSTRETTO_TAGS" fp "mykey"
    e `shouldBe` (Right "devkey")
  it "uses CONSTRETTO_TAGS dev,prod runtime property to get the right key" $ do
    setEnv "CONSTRETTO_TAGS" "prod,dev"
    cd <- getCurrentDirectory
    let fp = cd ++ "/test/testkonstretto.ini"
    e <- lookupFromEnvAndFile "CONSTRETTO_TAGS" fp "mykey"
    e `shouldBe` (Right "prodkey")
    