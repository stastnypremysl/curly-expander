module Main where

import Prelude
import Control.Monad
import System.Exit 
import Text.CurlyExpander

twoExpand :: Bool
twoExpand = (curlyExpand "car{{A,B},{C,D}}x" == ["carAx","carBx","carCx","carDx"])

multiExpand :: Bool
multiExpand = (curlyExpand "car{{A,B,C},D}" == ["carA","carB","carC","carD"])

rangeExpand :: Bool
rangeExpand = (curlyExpand "car{{0..2},{3..3}}" == ["car0","car1","car2","car3"])

main :: IO a
main = do
  when (not twoExpand) $ die "Test twoExpand failed"
  when (not multiExpand) $ die "Test multiExpand failed"
  when (not rangeExpand) $ die "Test rangeExpand failed"
 
  exitSuccess

  
