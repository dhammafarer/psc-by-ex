module Test.MySolutions where

import Data.Picture
import Prelude
import Data.Maybe (Maybe(..))
import ChapterExamples (Amp(..), Volt(..))

import Control.Bind (discard)

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

binomial :: Int -> Int -> Int
binomial _ 0 = 1
binomial 0 _ = 0
binomial n k | n < k = 0
  | otherwise = factorial n / (factorial k * (factorial (n - k)))

pascal :: Int -> Int -> Int
pascal _ 0 = 1
pascal 0 _ = 0
pascal n k
  = pascal (n-1) k + pascal (n - 1) (k - 1)

fromSingleton :: forall a. a -> Array a -> a
fromSingleton _ [b] = b
fromSingleton a _   = a

circleAtOrigin :: Shape
circleAtOrigin = Circle origin 10.0

centerShape :: Shape -> Shape
centerShape (Circle _ r) = Circle origin r
centerShape (Rectangle _ a b) = Rectangle origin a b
centerShape line@(Line p1 p2) = Line (p1 - delta) (p2 - delta)
  where
    delta = getCenter line
centerShape (Text _ s) = Text origin s

scaleShape :: Number -> Shape -> Shape
scaleShape n (Circle c r) = Circle c (r*n)
scaleShape n (Rectangle c a b) = Rectangle c (a*n) (b*n)
scaleShape n (Line p1 p2) = Line (p1*scale) (p2*scale)
  where
    scale = {x: n, y:n}
scaleShape _ text = text

doubleScaleAndCenter :: Shape -> Shape
doubleScaleAndCenter = centerShape <<< scaleShape 2.0

shapeText :: Shape -> Maybe String
shapeText (Text _ s) = Just s
shapeText _ = Nothing

newtype Watt = Watt Number

calculateWattage :: Amp -> Volt -> Watt
calculateWattage (Amp a) (Volt v) = Watt (a * v)
