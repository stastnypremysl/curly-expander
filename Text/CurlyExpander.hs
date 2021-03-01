{-|
Module      : Text.CurlyExpander
License     : GPL-3
Maintainer  : p@stty.cz
Stability   : testing
Portability : POSIX

This is the main module of the curly-expander package.

-}

{-# LANGUAGE OverloadedStrings #-}

module Text.CurlyExpander (curlyExpand) where

import qualified Data.Text as T
import qualified Data.Text.Lazy as L

import Data.Text.Lazy.Builder.Int (decimal)
import Data.Text.Lazy.Builder (toLazyText)

import Text.Parsec
import Text.Parsec.Text

import Data.Char


cumulatorComma :: Parser [L.Text]
cumulatorComma = do
  atoms <- (try p_range) <|> (try p_char_range) <|> p_atoms
  return atoms 
  
  where
    p_range :: Parser [L.Text]
    p_range = do
      nb1 <- many1 digit
      _ <- string ".."
      nb2 <- many1 digit

      return$ map (toLazyText . decimal) $ get_range (read nb1) (read nb2)
      where
        get_range :: Int -> Int -> [Int]
        get_range n1 n2
          | n1 > n2 = reverse$ get_range n2 n1
          | otherwise = [n1..n2]

    p_char_range :: Parser [L.Text]
    p_char_range = do
      char1 <- anyChar
      _ <- string ".."
      char2 <- anyChar
      
      return [ L.pack [p] | p <- get_range char1 char2 ]
      where
        get_range :: Char -> Char -> [Char]
        get_range c1 c2
          | n1 > n2 = reverse$ get_range c2 c1
          | otherwise = map chr [n1..n2]

          where
            n1 = ord c1
            n2 = ord c2

    p_atoms :: Parser [L.Text]
    p_atoms = do
      molecule <- many1$ try p_atom
      terminal_atom <- innerInputP
      return $ (concat molecule) ++ terminal_atom

    p_atom :: Parser [L.Text]
    p_atom = do

      atom <- innerInputP
      _ <- char ','
      return atom


bracketP :: Parser [L.Text]
bracketP = do

  _ <- char '{'
  ret <- cumulatorComma
  _ <- char '}'

  return$ ret

charP :: Parser [L.Text]
charP = do
  c <- anyChar
  return [L.pack [c]]

nonSpecialCharP :: Parser [L.Text]
nonSpecialCharP = do
  c <- noneOf ",}"
  return [L.pack [c]]

innerNonEmptyInputP :: Parser [L.Text]
innerNonEmptyInputP = do
  molecule <- (try bracketP <|> nonSpecialCharP)
  rest <- innerInputP

  return [ L.append a b | a <- molecule, b <- rest ]

innerInputP :: Parser [L.Text]
innerInputP = (innerNonEmptyInputP <|> emptyInputP)

nonEmptyInputP :: Parser [L.Text]
nonEmptyInputP = do
  molecule <- (try bracketP <|> charP)
  rest <- inputP

  return [ L.append a b | a <- molecule, b <- rest ]

emptyInputP :: Parser [L.Text]
emptyInputP = do
  return [""]

inputP :: Parser [L.Text]
inputP = (nonEmptyInputP <|> emptyInputP)
  

-- | Curly braces (brackets) expand function
--
-- First argument is a string, which you want to expand. Second argument is a list of expanded strings.
--
-- There are given few usage examples:
--
-- >>> curlyExpand "car{A,B}"
-- ["carA","carB"]
--
-- >>> curlyExpand "car{1..5}"
-- ["car1","car2","car3","car4","car5"]
--
-- >>> curlyExpand "car{{A,B},{C,D}}"
-- ["carA", "carB", "carC", "carD"]
--
-- >>> curlyExpand "{car,bus}{A..C}"
-- ["carA", "carB", "carC", "busA", "busB", "busC"]

curlyExpand :: T.Text -> [T.Text]
curlyExpand input =
  case parse inputP "bracket expansion"$ input of
    Left _ -> [input]
    Right ret -> map L.toStrict ret

