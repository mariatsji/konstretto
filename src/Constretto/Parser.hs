{-# LANGUAGE Safe #-}

{-|
Module      : Constretto.Parser
Description : Module uses Attoparsec to parse an .ini file
Copyright   : (c) Sjur Millidahl 2018
License     : MIT
Maintainer  : sjur.millidahl@gmail.com
Stability   : experimental
Portability : POSIX
-}
module Constretto.Parser
  ( constrettoParser
  , parse
  ) where

import qualified Data.Attoparsec.Text as AT
import qualified Data.Text            as T

import           Constretto.Internal.Types

type Keyval = (T.Text, T.Text)

data Block =
  Block Tag
        [Keyval]
  deriving (Eq, Show)

tagParser :: AT.Parser Tag
tagParser =
  AT.char '[' *> (tag <$> AT.takeWhile1 (/= ']')) <*
  AT.char ']' <* AT.endOfLine

keyParser :: AT.Parser T.Text
keyParser = AT.takeWhile1 (\c -> c /= '=' && c /= '[') <* AT.char '='

valParser :: AT.Parser T.Text
valParser = AT.takeWhile $ not . AT.isEndOfLine

keyvalParser :: AT.Parser [Keyval]
keyvalParser =
  AT.many' $ do
    key <- keyParser
    val <- valParser
    _ <- AT.many' AT.endOfLine
    return (key, val)

sectionParser' :: AT.Parser Block
sectionParser' = do
  tag' <- tagParser
  keyVals <- keyvalParser
  _ <- AT.many' AT.endOfLine
  return $ Block tag' keyVals

constrettoParser :: AT.Parser Constretto
constrettoParser = toConstretto <$> AT.many' sectionParser'

toConstretto :: [Block] -> Constretto
toConstretto blocks = mconcat $ toConstretto' <$> blocks

toConstretto' :: Block -> Constretto
toConstretto' (Block _ []) = emptyConstretto
toConstretto' (Block t ((k, v):xs)) = insert' t k v $ toConstretto' (Block t xs)

parse :: T.Text -> Either String Constretto
parse = AT.parseOnly constrettoParser