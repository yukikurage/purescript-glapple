module Graphics.Glapple.Util where

import Prelude

import Data.JSDate (getTime, now)
import Effect (Effect)
import Graphics.Canvas (CanvasElement, Transform)
import Math (cos, sin)
import Unsafe.Coerce (unsafeCoerce)
import Web.HTML (HTMLCanvasElement)

foreign import createCanvasElement :: Effect CanvasElement

canvasElementToHTMLCanvasElement :: CanvasElement -> HTMLCanvasElement
canvasElementToHTMLCanvasElement = unsafeCoerce

htmlCanvasElementToCanvasElement :: HTMLCanvasElement -> CanvasElement
htmlCanvasElementToCanvasElement = unsafeCoerce

getNowTime :: Effect Number
getNowTime = (_ / 1000.0) <<< getTime <$> now

---------------------------
-- Transform Computation --
---------------------------

foreign import multiplyTransform
  :: Transform -> Transform -> Transform

foreign import inverseTransform
  :: Transform -> Transform

foreign import transform
  :: Transform -> { x :: Number, y :: Number } -> { x :: Number, y :: Number }

-- | convert angle to Transform
rotate :: Number -> Transform
rotate x =
  { m11: cos x, m12: sin x, m21: -sin x, m22: cos x, m31: 0.0, m32: 0.0 }

-- | convert parallel movement to Transform
translate :: Number -> Number -> Transform
translate x y =
  { m11: 1.0, m12: 0.0, m21: 0.0, m22: 1.0, m31: x, m32: y }

scale :: Number -> Number -> Transform
scale x y =
  { m11: x, m12: 0.0, m21: 0.0, m22: y, m31: 0.0, m32: 0.0 }

unitTransform :: Transform
unitTransform = {m11: 1.0, m12: 0.0, m21: 0.0, m22: 1.0, m31: 0.0, m32: 0.0}

infixr 7 multiplyTransform as |*|
