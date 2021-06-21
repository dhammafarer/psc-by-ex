module Test.MySolutions where

import Prelude

import Data.Array (head, tail,filter)
import Data.Int (even)
import Data.Maybe (fromMaybe)

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

squared :: Array Number -> Array Number
squared [] = []
squared xs = (\x -> x*x) <$> xs

keepNonNegative :: Array Number -> Array Number
keepNonNegative = filter (\x -> x >= 0.0)

infix 5 filter as <$?>

keepNonNegativeRewrite :: Array Number -> Array Number
keepNonNegativeRewrite xs = (\x -> x >= 0.0) <$?> xs
