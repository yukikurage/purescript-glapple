module Graphics.Glapple.Data.Collider where

import Prelude

import Data.Array (any)
import Data.Foldable (maximum, minimum, or)
import Data.Maybe (fromMaybe)
import Data.Number (infinity)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import Debug (spy)
import Graphics.Glapple.Data.Complex (Complex, complex, image, magnitudeSqr, real, (:*))
import Graphics.Glapple.Data.Picture (Picture, Shape(..), arc, fan, rect)
import Graphics.Glapple.Data.Picture as P
import Graphics.Glapple.Data.Transform (Transform, computeChildTransform, inverseTransform, transform, translate)
import Math (pi, pow)

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

center :: Collider -> Complex
center = case _ of
  ColliderAppend c0 c1 -> 0.5 :* (center c0 + center c1)
  ColliderRect comp -> 0.5 :* comp
  ColliderCircle _ -> zero
  ColliderTransform trans c0 -> transform trans $ center c0
  ColliderEmpty -> zero

collide :: Collider -> Collider -> Boolean
collide c0 c1 = case c0 of
  ColliderAppend c2 c3 -> or $ map (\c -> collide c c1) [ c2, c3 ]
  ColliderRect comp -> case c1 of
    ColliderAppend c2 c3 ->
      or $ map (collide c0) [ c2, c3 ]
    ColliderRect _ -> true
    ColliderCircle _ -> true
    ColliderTransform trans (ColliderAppend c2 c3) ->
      or $ map
        (collide c0 <<< ColliderTransform trans)
        [ c2, c3 ]
    ColliderTransform trans (ColliderRect comp') -> f comp trans comp' && f
      comp'
      (inverseTransform trans)
      comp
      where
      f cmp trs cmp' = dx && dy
        where
        -- 後者の頂点
        vs = map (transform trs)
          [ zero, complex (real cmp') 0.0, complex 0.0 (image cmp'), cmp' ]
        -- x座標への射影
        sx = fromMaybe infinity (minimum (map real vs)) /\ fromMaybe (-infinity)
          (maximum (map real vs))
        -- y座標への射影
        sy = fromMaybe infinity (minimum (map image vs)) /\ fromMaybe
          (-infinity)
          (maximum (map image vs))
        -- x座標への射影がかぶっているか
        dx = fst sx <= real cmp && 0.0 <= snd sx
        -- y座標への射影がかぶっているか
        dy = fst sy <= image cmp && 0.0 <= snd sy
    ColliderTransform trans (ColliderCircle r) ->
      -r <= real t && real t <= real comp + r && 0.0 <= image t && image t <=
        image comp
        || 0.0 <= real t && real t <= real comp && -r <= image t && image t <=
          image comp + r
        || any
          (\cmp -> isCollidePosition cmp c1)
          [ zero, complex (real comp) 0.0, complex 0.0 (image comp), comp ]
      where
      t = translate trans
    ColliderTransform trans (ColliderTransform trans' c2) -> collide c0
      $ ColliderTransform (computeChildTransform trans trans')
      $ c2
    ColliderTransform _ ColliderEmpty -> false
    ColliderEmpty -> false
  ColliderCircle r -> case c1 of
    ColliderAppend c2 c3 ->
      or $ map (collide c0) [ c2, c3 ]
    ColliderRect _ -> true
    ColliderCircle _ -> true
    ColliderTransform trans (ColliderAppend c2 c3) ->
      or $ map
        (collide c0 <<< ColliderTransform trans)
        [ c2, c3 ]
    ColliderTransform trans (ColliderRect comp') ->
      -r <= real t && real t <= real comp' + r && 0.0 <= image t && image t <=
        image comp'
        || 0.0 <= real t && real t <= real comp' && -r <= image t && image t <=
          image comp' + r
        || any
          (\cmp -> isCollidePosition cmp c0)
          ( map (transform trans)
              [ zero
              , complex (real comp') 0.0
              , complex 0.0 (image comp')
              , comp'
              ]
          )
      where
      t = translate $ inverseTransform trans
    ColliderTransform trans (ColliderCircle r') -> magnitudeSqr t <= pow
      (r + r')
      2.0
      where
      t = translate trans
    ColliderTransform trans (ColliderTransform trans' c2) -> collide c0
      $ ColliderTransform (computeChildTransform trans trans')
      $ c2
    ColliderTransform _ ColliderEmpty -> false
    ColliderEmpty -> false
  ColliderTransform trans c2 -> collide c2
    (ColliderTransform (inverseTransform trans) c1)
  ColliderEmpty -> false

instance Semigroup Collider where
  append = ColliderAppend

instance Monoid Collider where
  mempty = ColliderEmpty

wireFrame :: forall sprite. Collider -> Picture sprite
wireFrame = case _ of
  ColliderAppend c0 c1 -> wireFrame c0 <> wireFrame c1
  ColliderRect comp -> rect Stroke comp
  ColliderCircle r -> arc { angle: 2.0 * pi, radius: r, start: 0.0 }
  ColliderTransform trans c -> P.transform trans $ wireFrame c
  ColliderEmpty -> mempty

filled :: forall sprite. Collider -> Picture sprite
filled = case _ of
  ColliderAppend c0 c1 -> filled c0 <> filled c1
  ColliderRect comp -> rect Fill comp
  ColliderCircle r -> fan Fill { angle: 2.0 * pi, radius: r, start: 0.0 }
  ColliderTransform trans c -> P.transform trans $ filled c
  ColliderEmpty -> mempty
