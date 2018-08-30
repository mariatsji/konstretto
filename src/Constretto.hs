{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe              #-}

{-|
Module      : Constretto
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
module Constretto
  ( readConfig
  , lookupC
  , lookupCFromEnv
  , Tag()
  ) where

import           Constretto.Internal.Types
import           Constretto.Parser
import qualified Data.Text                 as T
import qualified Data.Text.IO              as TIO
import           System.Environment

-- | `readConfig` accepts a filepath to a configuration ini-file and attempts to parse it
readConfig :: FilePath -> IO (Either String Constretto)
readConfig fp = parse <$> TIO.readFile fp

-- | 'lookupC' accepts `[Tag]` and a key and a configuratin file and give any associated value as a `Data.Text`
lookupC :: [Tag] -> T.Text -> Constretto -> Maybe T.Text
lookupC = lookup''

-- | 'lookupCFromEnv' calls `lookupC` with comma separated `[Tag]` from the system property CONSTRETTO_TAGS
lookupCFromEnv :: T.Text -> Constretto -> IO (Maybe T.Text)
lookupCFromEnv key constretto = do
  tags <- getTagsFromEnv
  pure $ lookup'' tags key constretto

getTagsFromEnv :: IO [Tag]
getTagsFromEnv = do
  asString <- lookupEnv "CONSTRETTO_TAGS"
  case asString of
    Nothing -> pure []
    Just s -> do
      let asText =
            T.pack $
            fmap
              (\c ->
                 if c == ','
                   then ' '
                   else c)
              s
          tags = tag <$> T.words asText
      pure tags