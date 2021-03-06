{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe              #-}

{-|
Module      : Konstretto.Internal.Types
Description : Types and internal utils for construction and lookup of a `Konstretto`
Copyright   : (c) Sjur Millidahl 2018
License     : MIT
Maintainer  : sjur.millidahl@gmail.com
Stability   : experimental
Portability : POSIX
-}
module Konstretto.Internal.Types
  ( emptyKonstretto
  , lookup'
  , lookup''
  , lookupFromEnv
  , splitStringOnComma
  , insert'
  , Tag()
  , tag
  , Konstretto(..)
  ) where

import qualified Data.Map.Strict    as Map
import           Data.Maybe
import           Data.Monoid        (Monoid)
import           Data.Semigroup     (Semigroup, (<>))
import           Data.String
import qualified Data.Text          as T
import           System.Environment

data Tag =
  Tag T.Text
  deriving (Eq, Ord, Show)

tag :: T.Text -> Tag
tag = Tag . trim

instance IsString Tag where
  fromString = Tag . T.pack

data Konstretto =
  Konstretto (Map.Map Tag (Map.Map T.Text T.Text))
  deriving (Eq, Show)

lookup' :: Tag -> T.Text -> Konstretto -> Maybe T.Text
lookup' tag' key (Konstretto mouter) =
  let tagLookup = mouter Map.!? tag' >>= (Map.lookup key)
      defaultLookup = mouter Map.!? "default" >>= (Map.lookup key)
   in first' tagLookup defaultLookup

lookup'' :: [Tag] -> T.Text -> Konstretto -> Maybe T.Text
lookup'' tags key c =
  let tagLookupsFromRight = foldr (\t _ -> lookup' t key c) Nothing tags
      defaultLookup = lookup' "default" key c
   in first' tagLookupsFromRight defaultLookup

lookupFromEnv :: T.Text -> String -> Konstretto -> IO (Maybe T.Text)
lookupFromEnv key env c = do
  maybeString <- lookupEnv env
  let tags = tag <$> splitMaybeStrings maybeString
  return $ lookup'' tags key c

splitMaybeStrings :: Maybe String -> [T.Text]
splitMaybeStrings Nothing  = []
splitMaybeStrings (Just s) = T.pack <$> splitStringOnComma s

splitStringOnComma :: String -> [String]
splitStringOnComma [] = []
splitStringOnComma (',':xs) = splitStringOnComma xs
splitStringOnComma s =
  (takeWhile (/= ',') s) : (splitStringOnComma $ dropWhile (/= ',') s)

insert' :: Tag -> T.Text -> T.Text -> Konstretto -> Konstretto
insert' tag' key val (Konstretto outer) =
  let k = trim key
      v = trim val
      inner' = fromMaybe Map.empty (Map.lookup tag' outer)
      inner = Map.insert k v inner'
      outer' = Map.insert tag' inner outer
   in Konstretto outer'

emptyKonstretto :: Konstretto
emptyKonstretto = Konstretto Map.empty

trim :: T.Text -> T.Text
trim = T.reverse . trim' . T.reverse . trim'

trim' :: T.Text -> T.Text
trim' t
  | T.null t = T.empty
  | isSpecialChar (T.head t) = trim' $ T.tail t
  | otherwise = t
  where
    isSpecialChar x = x == ' ' || x == '\r' || x == '\t' || x == '\n'

first' :: Maybe a -> Maybe a -> Maybe a
first' a@(Just _) _ = a
first' _ b          = b

instance Semigroup Konstretto where
  (Konstretto m1) <> (Konstretto m2) = Konstretto $ m1 <> m2

instance Monoid Konstretto where
  mempty = emptyKonstretto
  mappend = (<>)
