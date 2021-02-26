{-|
Module      : Text.CurlyExpander
License     : GPL-3
Maintainer  : p@stty.cz
Stability   : testing
Portability : POSIX

This is the main module of the curly-expander package.

-}

module Text.CurlyExpander (curlyExpand) where

import qualified Data.Text as T

import Text.Parsec
import Text.Parsec.Text

import Data.Char


cumulatorComma :: Parser [String]
cumulatorComma = do
  atoms <- (try p_range) <|> (try p_char_range) <|> p_atoms
  return atoms 
  
  where
    p_range :: Parser [String]
    p_range = do
      nb1 <- many1 digit
      _ <- string ".."
      nb2 <- many1 digit

      return$ map (show) $ get_range (read nb1) (read nb2)
      where
        get_range :: Int -> Int -> [Int]
        get_range n1 n2
          | n1 > n2 = reverse$ get_range n2 n1
          | otherwise = [n1..n2]

    p_char_range :: Parser [String]
    p_char_range = do
      char1 <- anyChar
      _ <- string ".."
      char2 <- anyChar
      
      return [[p] | p <- get_range char1 char2 ]
      where
        get_range :: Char -> Char -> [Char]
        get_range c1 c2
          | n1 > n2 = reverse$ get_range c2 c1
          | otherwise = map chr [n1..n2]

          where
            n1 = ord c1
            n2 = ord c2

    p_atoms :: Parser [String]
    p_atoms = do
      molecule <- many1$ try p_atom
      terminal_atom <- innerInputP
      return $ (concat molecule) ++ terminal_atom

    p_atom :: Parser [String]
    p_atom = do

      atom <- innerInputP
      _ <- char ','
      return atom


bracketP :: Parser [String]
bracketP = do

  _ <- char '{'
  ret <- cumulatorComma
  _ <- char '}'

  return$ ret

charP :: Parser [String]
charP = do
  c <- anyChar
  return [[c]]

nonSpecialCharP :: Parser [String]
nonSpecialCharP = do
  c <- noneOf ",}"
  return [[c]]

innerNonEmptyInputP :: Parser [String]
innerNonEmptyInputP = do
  molecule <- (try bracketP <|> nonSpecialCharP)
  rest <- innerInputP

  return [ a ++ b | a <- molecule, b <- rest ]

innerInputP :: Parser [String]
innerInputP = (innerNonEmptyInputP <|> emptyInputP)

nonEmptyInputP :: Parser [String]
nonEmptyInputP = do
  molecule <- (try bracketP <|> charP)
  rest <- inputP

  return [ a ++ b | a <- molecule, b <- rest ]

emptyInputP :: Parser [String]
emptyInputP = do
  return [""]

inputP :: Parser [String]
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

curlyExpand :: String -> [String]
curlyExpand input =
  case parse inputP "bracket expansion"$ T.pack input of
    Left _ -> [input]
    Right ret -> ret

