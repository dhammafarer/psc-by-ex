module Test.MySolutions where

import Prelude

import Data.Path
import Data.Array (head,tail,filter,cons,concatMap,(:),length,(..))
import Data.Int (even,rem,quot)
import Data.Maybe (Maybe,fromMaybe)
import Test.Examples (factors)
import Control.MonadZero (guard)
import Test.Examples (allFiles)

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

isPrime :: Int -> Boolean
isPrime n = if n < 2
              then false
              else length (factors n) == 1

cartesianProduct :: ∀ a. Array a -> Array a -> Array (Array a)
cartesianProduct xs ys = do
  x <- xs
  y <- ys
  pure [ x, y ]

triples :: Int -> Array (Array Int)
triples n = do
  a <- 1..n
  b <- a..n
  c <- b..n
  guard $ a*a + b*b == c*c
  pure [a,b,c]

factorize :: Int -> Array Int
factorize n = factorize' 2 n []
  where
  factorize' :: Int -> Int -> Array Int -> Array Int
  factorize' _ 1 result = result

  factorize' divisor dividend result =
    let
      remainder = rem dividend divisor
    in
      if remainder == 0 then
        factorize' (divisor) (quot dividend divisor) (cons divisor result)
      else
        factorize' (divisor + 1) dividend result

onlyFiles :: Path -> Array Path
onlyFiles = filter (not isDirectory) <<< allFiles

whereIs :: Path -> String -> Maybe Path
whereIs path name = head $ whereIs' $ allFiles path
  where
    matchName :: String -> Path -> Path -> Boolean
    matchName name dir file = (show dir) <> name == show file
    whereIs' :: Array Path -> Array Path
    whereIs' paths = do
       child <- paths
       f <- ls child
       guard $ matchName name child f
       pure child

