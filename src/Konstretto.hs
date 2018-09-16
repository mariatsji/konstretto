{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe              #-}

{-|
Module      : Konstretto
Description : Module for endusers to import
Copyright   : (c) Sjur Millidahl 2018
License     : MIT
Maintainer  : sjur.millidahl@gmail.com
Stability   : experimental
Portability : POSIX

This module can read a configuration file written like

@
  [default]
  mykey = defaultval

  [dev]
  dbaddress = dev.db.org
  mykey     = devkey

  [prod]
  dbaddress = prod.db.org
  mykey = prodkey
@

keys and values are separated by `=`

the [default] tag is special as it does not require being listed in lookups, but key-values listed under [default] will still
provide a match

The configuration file format does not support comments (in this version)
-}
module Konstretto
  ( readConfig
  , lookupC
  , lookupCFromEnv
  , lookupFromTagsAndFile
  , lookupFromEnvAndFile
  , Tag()
  , Konstretto()
  , EnvTags
  ) where

import           Konstretto.Internal.Types
import           Konstretto.Parser
import qualified Data.Text                 as T
import qualified Data.Text.IO              as TIO
import           System.Environment

-- | The name of the runtime system property to interpret as a list of tags in descending orders
type EnvTags = String

-- | `readConfig` accepts a filepath to a configuration ini-file and attempts to parse it strictly
readConfig :: FilePath -> IO (Either String Konstretto)
readConfig fp = parse <$> TIO.readFile fp

-- | 'lookupC' accepts `[Tag]` and a key and a configuratin file and give any associated value as a `Data.Text`
lookupC :: Konstretto -> [Tag] -> T.Text -> Maybe T.Text
lookupC konstretto tags key = lookup'' tags key konstretto

-- | 'lookupCFromEnv' calls `lookupC` with comma separated `[Tag]` from the system property `envKey`
lookupCFromEnv :: Konstretto -> EnvTags -> T.Text -> IO (Maybe T.Text)
lookupCFromEnv konstretto envKey key =
  fmap (\tags -> lookup'' tags key konstretto) (getTagsFromEnv envKey)

-- | 'lookupFromEnvAndFile' parses a given config file and searches for values given keys and tags from envKey runtime property
lookupFromEnvAndFile :: EnvTags -> FilePath -> T.Text -> IO (Either String T.Text)
lookupFromEnvAndFile envKey fp key = do
  tags <- getTagsFromEnv envKey
  lookupFromTagsAndFile fp tags key

-- | 'lookupFromTagsAndFile' parses a given config file and searches for values given keys and tags (searched from right)
lookupFromTagsAndFile :: FilePath -> [Tag] -> T.Text -> IO (Either String T.Text)
lookupFromTagsAndFile fp tags key =
  fmap maybeToLeft' $ fmap (\kon -> lookupC kon tags key) <$> readConfig fp
  
maybeToLeft' :: Either String (Maybe T.Text) -> Either String T.Text
maybeToLeft' (Right Nothing) = Left "cannot find key"
maybeToLeft' (Right (Just r)) = Right r
maybeToLeft' (Left e) = Left e

getTagsFromEnv :: String -> IO [Tag]
getTagsFromEnv s =
  maybe [] (fmap tag . T.words . T.pack . commaToSpace) <$>
  lookupEnv s

commaToSpace :: String -> String
commaToSpace =
  fmap
    (\c ->
       if c == ','
         then ' '
         else c)
