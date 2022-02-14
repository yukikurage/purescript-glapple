module Graphics.Glapple.Data.Collider where

import Prelude

import Graphics.Canvas (Transform)
import Graphics.Glapple.Util (inverseTransform, transform)

data Collider
  = ColliderAppend Collider Collider
  | ColliderRect Number Number
  | ColliderCircle Number
  | ColliderTransform Transform Collider
  | ColliderEmpty

isCollidePosition :: Number -> Number -> Collider -> Boolean
isCollidePosition x y = case _ of
  ColliderAppend c0 c1 -> isCollidePosition x y c0 && isCollidePosition x y c1
  ColliderRect w h -> 0.0 <= x && x <= w && 0.0 <= y && y <= h
  ColliderCircle r -> x * x + y * y <= r * r
  ColliderTransform trans c -> isCollidePosition resX resY c
    where
    { x: resX, y: resY } = transform (inverseTransform trans) { x, y }
  ColliderEmpty -> false

instance Semigroup Collider where
  append = ColliderAppend

instance Monoid Collider where
  mempty = ColliderEmpty
