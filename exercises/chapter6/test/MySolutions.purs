module Test.MySolutions where

import Prelude

-- Note to reader: Add your solutions to this file
newtype Point
  = Point
  { x :: Number
  , y :: Number
  }

instance Show Point where
  show (Point {x,y}) = "(" <> show x <> ", " <> show y <> ")"

newtype Complex
  = Complex
  { real :: Number
  , imaginary :: Number
  }

derive instance eqComplex :: Eq Complex

instance Show Complex where
  show (Complex {real,imaginary}) = show real <> sign <> show imaginary <> "i"
    where
      sign | imaginary < 0.0 = ""
           | otherwise       = "+"
