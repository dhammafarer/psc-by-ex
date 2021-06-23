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

instance Semiring Complex where
  add (Complex {real: r1, imaginary: i1}) (Complex {real: r2, imaginary: i2}) = Complex {real: (r1+r2), imaginary: (i1+i2)}
  mul (Complex {real: r1, imaginary: i1}) (Complex {real: r2, imaginary: i2}) = Complex {real: (r1 * r2 - i1 * i2), imaginary: (r1 * i2 + r2 * i1)}
  zero = Complex {real: 0.0, imaginary: 0.0}
  one = Complex {real: 1.0, imaginary: zero}

data NonEmpty a
  = NonEmpty a (Array a)

instance eqNonEmpty :: Eq a => Eq (NonEmpty a) where
  eq (NonEmpty e1 a1) (NonEmpty e2 a2) = e1 == e2 && a1 == a2

instance showNonEmpty :: Show a => Show (NonEmpty a) where
  show (NonEmpty e1 a1) = show e1 <> "," <> show a1
