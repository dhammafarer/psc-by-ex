module Test.MySolutions where

import Prelude
import Data.Maybe(fromMaybe)
import Data.Int(even)
import Data.Array(head,tail)

-- Note to reader: Add your solutions to this file

isEven :: Int -> Boolean
isEven n =
  if n < 0
    then isEven (-n)
    else if n == 0
      then true
      else not (isEven (n - 1))

countEven :: Array Int -> Int
countEven [] = 0
countEven xs = isEvenHead + isEvenTail
  where oneIfEven n = if even n then 1 else 0
        isEvenHead = (oneIfEven $ fromMaybe 1 $ head xs)
        isEvenTail = (countEven $ fromMaybe [] $ tail xs)
