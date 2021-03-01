{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude
import Control.Monad
import System.Exit 
import Text.CurlyExpander

noInput :: Bool
noInput = (curlyExpand "" == [""])

pureExpand :: Bool
pureExpand = (curlyExpand "{,a,}" == ["", "a", ""])

noExpand :: Bool
noExpand = (curlyExpand "car" == ["car"])

emptyExpand :: Bool
emptyExpand = (curlyExpand "ca{}r" == ["ca{}r"])

twoExpand :: Bool
twoExpand = (curlyExpand "car{A,B}x" == ["carAx","carBx"])

multiExpand :: Bool
multiExpand = (curlyExpand "car{A,B,C}" == ["carA","carB","carC"])

cartersianExpand :: Bool
cartersianExpand = (curlyExpand "c{A,B}{A,B}c" == ["cAAc","cABc","cBAc", "cBBc"])

main :: IO a
main = do
  when (not noInput) $ die "Test noInput failed"

  when (not pureExpand) $ die "Text pureExpand failed"

  when (not noExpand) $ die "Test noExpand failed"
  when (not emptyExpand) $ die "Test emptyExpand failed"

  when (not twoExpand) $ die "Test twoExpand failed"
  when (not multiExpand) $ die "Test multiExpand failed"
  
  when (not cartersianExpand) $ die "Test cartersianExpand failed"

  exitSuccess

  
