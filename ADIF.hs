{-# LANGUAGE NoMonomorphismRestriction #-}

module ADIF (
        adif,
        adifFromFile 
        ) where

import Control.Monad
import Data.List (unfoldr)
import Data.Maybe (isNothing, isJust)
import Data.Map (fromList)
import Text.Parsec
import Text.Parsec.Char
--import Text.Parsec.String
import Text.Parsec.ByteString

type Field = (String,String)
type Record = [Field]

adifFromFile = parseFromFile adif

adif = fmap gather adif_stream

gather = unfoldr f . dropWhile isNothing  where
        f [] = Nothing
        f l  = Just (fromList (fromMaybes l), 
                     dropWhile isNothing . dropWhile isJust $ l)
        
fromMaybes (Just x : xs) = x : fromMaybes xs
fromMaybes _             = []

adif_stream  = pad >> (field `endBy` many (noneOf "<")) 

pad = many (noneOf "<")

field = char '<' >> (try ( string "EOR>"   >> return Nothing) <|>
                     try ( string "eor>"   >> return Nothing) <|>
                     try ( string "eoh>"   >> return Nothing) <|>
                     do
                        key <- many1 $ noneOf ":>"
                        char ':'
                        len <- read `fmap` many1 digit
                        char '>'
                        val <- replicateM len anyChar
                        return $ Just (key, val))




