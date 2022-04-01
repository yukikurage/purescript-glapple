module Graphics.Glapple.Data.Complex where

import Prelude

import Math (cos, sin, sqrt)

-- | 複素数型
newtype Complex = Complex
  { real :: Number
  , image :: Number
  }

derive newtype instance Eq Complex
derive newtype instance Ord Complex
derive newtype instance Show Complex

instance Semiring Complex where
  add (Complex { real: r0, image: i0 }) (Complex { real: r1, image: i1 }) =
    Complex { real: r0 + r1, image: i0 + i1 }
  zero = Complex { real: 0.0, image: 0.0 }
  one = Complex { real: 1.0, image: 0.0 }
  mul (Complex { real: r0, image: i0 }) (Complex { real: r1, image: i1 }) =
    Complex { real: r0 * r1 - i0 * i1, image: r0 * i1 + r1 * i0 }

instance Ring Complex where
  sub (Complex { real: r0, image: i0 }) (Complex { real: r1, image: i1 }) =
    Complex { real: r0 - r1, image: i0 - i1 }

instance CommutativeRing Complex

instance EuclideanRing Complex where
  degree = const 1
  div (Complex { real: r0, image: i0 }) (Complex { real: r1, image: i1 }) =
    Complex
      { real: (r0 * r1 + i0 * i1) / (r1 * r1 + i1 * i1)
      , image: (i0 * r1 - r0 * i1) / (r1 * r1 + i1 * i1)
      }
  mod = const zero

complex :: Number -> Number -> Complex
complex x y = Complex { real: x, image: y }

magnitudeSqr :: Complex -> Number
magnitudeSqr (Complex { real: r, image: i }) = r * r + i * i

multiplyScalar :: Number -> Complex -> Complex
multiplyScalar x (Complex { real: r, image: i }) = Complex
  { real: r * x, image: i * x }

infix 7 multiplyScalar as :*

real :: Complex -> Number
real (Complex { real }) = real

image :: Complex -> Number
image (Complex { image }) = image

rotateComplex :: Number -> Complex
rotateComplex theta = Complex
  { real: cos theta
  , image: sin theta
  }

i :: Complex
i = complex 0.0 1.0

normalize :: Complex -> Complex
normalize comp = complex (real comp / magnitude) (image comp / magnitude)
  where
  magnitude = sqrt (magnitudeSqr comp)
