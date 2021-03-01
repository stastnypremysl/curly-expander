{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude
import Control.Monad
import System.Exit 
import Text.CurlyExpander

noExpand :: Bool
noExpand = (curlyExpand "car{0..}" == ["car{0..}"])

emptyExpand :: Bool
emptyExpand = (curlyExpand "ca{0..0}r" == ["ca0r"])

twoExpand :: Bool
twoExpand = (curlyExpand "car{0..1}x" == ["car0x","car1x"])

multiExpand :: Bool
multiExpand = (curlyExpand "car{3..5}" == ["car3","car4","car5"])

charExpand :: Bool
charExpand = (curlyExpand "car{a..b}" == ["cara","carb"])

main :: IO a
main = do
  when (not noExpand) $ die "Test noExpand failed"
  when (not emptyExpand) $ die "Test emptyExpand failed"

  when (not twoExpand) $ die "Test twoExpand failed"
  when (not multiExpand) $ die "Test multiExpand failed"
  
  when (not charExpand) $ die "Test charExpand failed"

  exitSuccess

  
