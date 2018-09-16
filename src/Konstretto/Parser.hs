{-# LANGUAGE Safe #-}

{-|
Module      : Konstretto.Parser
Description : Module uses Attoparsec to parse an .ini file
Copyright   : (c) Sjur Millidahl 2018
License     : MIT
Maintainer  : sjur.millidahl@gmail.com
Stability   : experimental
Portability : POSIX
-}
module Konstretto.Parser
  ( konstrettoParser
  , parse
  ) where

import qualified Data.Attoparsec.Text as AT
import qualified Data.Text            as T

import           Konstretto.Internal.Types

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

konstrettoParser :: AT.Parser Konstretto
konstrettoParser = toKonstretto <$> AT.many' sectionParser'

toKonstretto :: [Block] -> Konstretto
toKonstretto blocks = mconcat $ toKonstretto' <$> blocks

toKonstretto' :: Block -> Konstretto
toKonstretto' (Block _ []) = emptyKonstretto
toKonstretto' (Block t ((k, v):xs)) = insert' t k v $ toKonstretto' (Block t xs)

parse :: T.Text -> Either String Konstretto
parse = AT.parseOnly konstrettoParser