{-|
Module      : Text.CurlyExpander
License     : LGPL-3
Maintainer  : p-w@stty.cz
Stability   : testing
Portability : POSIX

This is the main (and only) module of the curly-expander package.

-}

{-# LANGUAGE OverloadedStrings #-}

module Text.CurlyExpander 
  (
    curlyExpand, 
    BackslashConfig (NoHandle, Preserve, Standard), 
    ExpandConfig (ExpandConfig, quotePairs, backslashConfig, persistQuotePairs, allowOneElementExpand), 
    defaultExpandConfig, 
    customCurlyExpand
  ) 
where

import qualified Data.Text as T
import qualified Data.Text.Lazy as L

import Data.Text.Lazy.Builder.Int (decimal)
import Data.Text.Lazy.Builder (toLazyText)

import Text.Parsec
import Text.Parsec.Text

import Data.Char

-- | This configuration specify, how should be backslashes handled.
-- It is part of `ExpandConfig`.
data BackslashConfig = 
    -- | If no handle is used, then backslashes are not handled in any special way.
    NoHandle | 

    -- | If preserve is used, backslashes are processed, any backslashed char is processed as nonspecial char 
    -- and backslashes aren't deleted from result.
    Preserve | 

    -- | If standard is used, backslashes are processed, any backslashed char is processed as nonspecial char
    -- and backslashes are deleted from result.
    Standard
  deriving Eq


-- | The curly braces expand config. 
-- It is used in `customCurlyExpand`.
data ExpandConfig = ExpandConfig {
    -- | The configuration, which defines, how should be backslashes handled (\\)
    backslashConfig :: BackslashConfig,

    -- | Quote pairs, which encloses a substrings, tells expander, that the substring shouldn't be expanded.
    -- For example (\"[\", \"]\") pairs tells to expander, that anything inside [ANYTHING] shouldn't be expanded.
    quotePairs :: [(String, String)],

    -- | If true, quote pairs aren't deleted. Otherwise they are deleted from a result.
    persistQuotePairs :: Bool,

    -- | If true, curly brackets around one element will be deleted. Otherwise they are persisted.
    allowOneElementExpand :: Bool
  }


-- | The default curly braces expand function config.
-- By default backslashes are not handeled, there are no quote pairs and one element expand is forbidden.
-- See the source code for details.
defaultExpandConfig :: ExpandConfig
defaultExpandConfig = ExpandConfig { 
    backslashConfig = NoHandle, 
    quotePairs = [],
    persistQuotePairs = False,
    allowOneElementExpand = False
  }

-- | Custom curly braces (brackets) expand function.
-- It works in the same way as curlyExpand, bud accept custom configuration `ExpandConfig` in the first argument.

customCurlyExpand :: ExpandConfig -> T.Text -> [T.Text]
customCurlyExpand config input =
  case parse inputP "bracket expansion"$ input of
    Left _ -> [input]
    Right ret -> map L.toStrict ret

  where 
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
          molecule <- moleculeP
          terminal_atom <- innerInputP
          return $ (concat molecule) ++ terminal_atom

         where 
            moleculeP :: Parser [[L.Text]]
            moleculeP = 
             if allowOneElementExpand config; then
               many (try p_atom)
             else
               many1 (try p_atom)

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

    backslashedP :: Parser [L.Text]
    backslashedP = do
      if handleBackslash then do
        _ <- char '\\'
        c <- anyChar

        return$ getReturnValue c
      else do
        unexpected "Char is not backslashed."

      where
        handleBackslash :: Bool
        handleBackslash = 
          if backslashConfig config == NoHandle then
            False
          else 
            True

        getReturnValue :: Char -> [L.Text]
        getReturnValue c =
          if backslashConfig config == Preserve then
            [ L.pack ['\\', c] ]
          else
            [ L.pack [c] ]

    specialQuotedP :: (String, String) -> Parser [L.Text]
    specialQuotedP (lQuote,rQuote) = do
      _ <- string lQuote
      ret <- quoteNext

      return$ [enrichReturnValue ret]
      where
        quoteClosure :: Parser L.Text
        quoteClosure = do
          _ <- string rQuote
          return ""

        quoteNextChar :: Parser L.Text
        quoteNextChar = do
          c <- anyChar
          rest <- quoteNext
          return$ L.pack [c] `L.append` rest

        quoteNext :: Parser L.Text
        quoteNext = (try quoteClosure <|> quoteNextChar)

        enrichReturnValue :: L.Text -> L.Text
        enrichReturnValue ret = 
          if persistQuotePairs config; then
            (L.pack lQuote) `L.append` ret `L.append` (L.pack rQuote) 
          else
            ret

        
    quotedP :: [(String, String)] -> Parser [L.Text]
    quotedP (quotes : rest) = (try$ specialQuotedP quotes) <|> quotedP rest
    quotedP [] = unexpected "String is not quoted."

    allQuotedP :: Parser [L.Text]
    allQuotedP = quotedP$ quotePairs config

    innerNonEmptyInputP :: Parser [L.Text]
    innerNonEmptyInputP = do
      molecule <- (backslashedP <|> try allQuotedP <|> try bracketP <|> nonSpecialCharP)
      rest <- innerInputP

      return [ L.append a b | a <- molecule, b <- rest ]

    innerInputP :: Parser [L.Text]
    innerInputP = (innerNonEmptyInputP <|> emptyInputP)

    nonEmptyInputP :: Parser [L.Text]
    nonEmptyInputP = do
      molecule <- (backslashedP <|> try allQuotedP <|> try bracketP <|> charP)
      rest <- inputP

      return [ L.append a b | a <- molecule, b <- rest ]

    emptyInputP :: Parser [L.Text]
    emptyInputP = do
      return [""]

    inputP :: Parser [L.Text]
    inputP = (nonEmptyInputP <|> emptyInputP)

-- | Curly braces (brackets) expand function
--
-- First argument is a `Data.Text`, which you want to expand. Second argument is a list of expanded `Data.Text`s.
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
--
-- Be aware, that these examples will run only with `OverloadedStrings` language extension and proper `Data.Text` imports.

curlyExpand :: T.Text -> [T.Text]
curlyExpand input =
  customCurlyExpand defaultExpandConfig input

