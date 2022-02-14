module Graphics.Glapple.Data.Collider where

import Prelude

import Graphics.Glapple.Data.Complex (Complex, image, magnitudeSqr, real)
import Graphics.Glapple.Data.Transform (Transform, inverseTransform, transform)

data Collider
  = ColliderAppend Collider Collider
  | ColliderRect Complex
  | ColliderCircle Number
  | ColliderTransform Transform Collider
  | ColliderEmpty

isCollidePosition :: Complex -> Collider -> Boolean
isCollidePosition comp = case _ of
  ColliderAppend c0 c1 -> isCollidePosition comp c0 || isCollidePosition comp c1
  ColliderRect comp' -> 0.0 <= real comp && real comp <= real comp'
    && 0.0 <= image comp
    &&
      image comp <= image comp'
  ColliderCircle r -> magnitudeSqr comp <= r * r
  ColliderTransform trans c -> isCollidePosition
    (transform (inverseTransform trans) comp)
    c
  ColliderEmpty -> false

instance Semigroup Collider where
  append = ColliderAppend

instance Monoid Collider where
  mempty = ColliderEmpty
