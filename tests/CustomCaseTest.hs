{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude
import Control.Monad
import System.Exit 
import Text.CurlyExpander

import qualified Data.Text as T

denyOneElement :: Bool
denyOneElement = (curlyExpand "{a}" == ["{a}"])

allowOneElement :: Bool
allowOneElement = (expand "{a}" == ["a"])
  where
    expand :: T.Text -> [T.Text]
    expand = customCurlyExpand (defaultExpandConfig {allowOneElementExpand = True})

escapedString :: T.Text
escapedString = "\\ab\\c"

escapedString2 :: T.Text
escapedString2 = "a{1,2}bc\\{,5}"

noEscaping :: Bool
noEscaping = (curlyExpand escapedString == [escapedString])

preserveEscaping :: Bool
preserveEscaping = (expand escapedString == [escapedString])
  where
    expand :: T.Text -> [T.Text]
    expand = customCurlyExpand (defaultExpandConfig {backslashConfig = Preserve})

standardEscapeConfig :: ExpandConfig
standardEscapeConfig = defaultExpandConfig {backslashConfig = Standard}

standardEscaping :: Bool
standardEscaping = (customCurlyExpand standardEscapeConfig escapedString == ["abc"])

standardEscaping2 :: Bool
standardEscaping2 = (customCurlyExpand standardEscapeConfig escapedString2 == ["a1bc{,5}", "a2bc{,5}"])

quotedString1 :: T.Text
quotedString1 = "abc''abc''abc"

quotedString2 :: T.Text
quotedString2 = "a[ab]cc"

quotedString3 :: T.Text
quotedString3 = "a[{a,b}]c"

quotedString4 :: T.Text
quotedString4 = "a{b,[{1..2}]}c"

qPairs1 :: [(String, String)]
qPairs1 = [("[", "]")]

qPairs2 :: [(String, String)]
qPairs2 = qPairs1 ++ [("''", "''")]

queted1PersistE :: T.Text -> [T.Text]
queted1PersistE = customCurlyExpand (defaultExpandConfig {persistQuotePairs = True, quotePairs = qPairs1})

queted1NopersistE :: T.Text -> [T.Text]
queted1NopersistE = customCurlyExpand (defaultExpandConfig {persistQuotePairs = False, quotePairs = qPairs1})

queted2PersistE :: T.Text -> [T.Text]
queted2PersistE = customCurlyExpand (defaultExpandConfig {persistQuotePairs = True, quotePairs = qPairs2})

queted2NopersistE :: T.Text -> [T.Text]
queted2NopersistE = customCurlyExpand (defaultExpandConfig {persistQuotePairs = False, quotePairs = qPairs2})

queted1Persist1 :: Bool
queted1Persist1 = ( queted1PersistE quotedString1 == [quotedString1] )

queted1Nopersist1 :: Bool
queted1Nopersist1 = ( queted1NopersistE quotedString1 == [quotedString1] )

queted2Persist1 :: Bool
queted2Persist1 = ( queted2PersistE quotedString1 == [quotedString1] )

queted2Nopersist1 :: Bool
queted2Nopersist1 = ( queted2NopersistE quotedString1 == ["abcabcabc"] )

queted2Persist2 :: Bool
queted2Persist2 = ( queted2PersistE quotedString2 == [quotedString2] )

queted2Nopersist2 :: Bool
queted2Nopersist2 = ( queted2NopersistE quotedString2 == ["aabcc"] )

queted2Persist3 :: Bool
queted2Persist3 = ( queted2PersistE quotedString3 == [quotedString3] )

queted2Nopersist4 :: Bool
queted2Nopersist4 = ( queted2NopersistE quotedString4 == ["abc", "a{1..2}c"] )

main :: IO a
main = do
  when (not denyOneElement) $ die "Test denyOneElement failed"
  when (not allowOneElement) $ die "Test allowOneElement failed"

  when (not noEscaping) $ die "Test noEscaping failed"
  when (not preserveEscaping) $ die "Test preserveEscaping failed"
  when (not standardEscaping) $ die "Test standardEscaping failed"
  when (not standardEscaping2) $ die "Test standardEscaping2 failed"

  when (not queted1Persist1) $ die "Test queted1Persist1 failed"
  when (not queted1Nopersist1) $ die "Test queted1Nopersist1 failed"
  when (not queted2Persist1) $ die "Test queted2Persist1 failed"
  when (not queted2Nopersist1) $ die "Test queted2Nopersist1 failed"

  when (not queted2Persist2) $ die "Test queted2Persist2 failed"
  when (not queted2Nopersist2) $ die "Test queted2Nopersist2 failed"

  when (not queted2Persist3) $ die "Test queted2Persist3 failed"

  when (not queted2Nopersist4) $ die "Test queted2Nopersist4 failed"


  exitSuccess

  
