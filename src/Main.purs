module Main where

import Prelude

import Data.Array as Array
import Data.Foldable (sum)

isMultiple :: Int -> Boolean
isMultiple n = mod n 3 == 0 || mod n 5 == 0

answerA min max = sum $ Array.filter isMultiple $ Array.range min max
