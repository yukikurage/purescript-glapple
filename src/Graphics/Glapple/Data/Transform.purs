module Graphics.Glapple.Data.Transform where

import Prelude

import Graphics.Canvas as Canvas
import Graphics.Glapple.Data.Complex (Complex(..))
import Math (cos, sin)

-- | コンポーネントの Transform を表す型
-- | 適用順は Rotate -> Translate
newtype Transform = Transform
  { translate :: { x :: Number, y :: Number }
  , rotate :: Number
  }

derive newtype instance Eq Transform
derive newtype instance Ord Transform
derive newtype instance Show Transform

unitTransform :: Transform
unitTransform = Transform
  { translate: { x: 0.0, y: 0.0 }, rotate: 0.0 }

toCanvasTransform :: Transform -> Canvas.Transform
toCanvasTransform
  ( Transform
      { translate: { x: trX, y: trY }, rotate: rt }
  ) =
  { m11: cos rt
  , m12: sin rt
  , m21: -sin rt
  , m22: cos rt
  , m31: trX
  , m32: trY
  }

-- | 親，子の順番で Transform を渡して，子のグローバルなTransformを計算する
computeChildTransform :: Transform -> Transform -> Transform
computeChildTransform
  ( Transform
      { translate: { x: tr0X, y: tr0Y }
      , rotate: rt0
      }
  )
  ( Transform
      { translate: { x: tr1X, y: tr1Y }
      , rotate: rt1
      }
  ) =
  Transform
    { translate:
        { x: tr0X + tr1X * cos rt0 - tr1Y * sin rt0
        , y: tr0Y + tr1Y * cos rt0 + tr1X * sin rt0
        }
    , rotate: rt0 + rt1
    }

transform :: Transform -> Complex -> Complex
transform
  ( Transform
      { translate: { x: trX, y: trY }
      , rotate: rt
      }
  )
  (Complex { real: x, image: y }) = Complex
  { real: x * cos rt - y * sin rt + trX
  , image: x * sin rt + y * cos rt + trY
  }

fromTranslate :: Complex -> Transform
fromTranslate (Complex { real: x, image: y }) = Transform
  { translate: { x: x, y: y }
  , rotate: 0.0
  }

fromRotate :: Number -> Transform
fromRotate rt = Transform
  { translate: { x: 0.0, y: 0.0 }
  , rotate: rt
  }

modifyTranslate :: Complex -> Transform -> Transform
modifyTranslate
  (Complex { real: x, image: y })
  (Transform { rotate }) = Transform
  { translate: { x: x, y: y }
  , rotate
  }

modifyRotate :: Number -> Transform -> Transform
modifyRotate rt (Transform { translate }) = Transform
  { translate
  , rotate: rt
  }

translate :: Transform -> Complex
translate (Transform { translate: { x: trX, y: trY } }) = Complex
  { real: trX
  , image: trY
  }

rotate :: Transform -> Number
rotate (Transform { rotate: rt }) = rt

inverseTransform :: Transform -> Transform
inverseTransform (Transform { translate: { x: trX, y: trY }, rotate: rt }) =
  Transform
    { translate:
        { x: -trX * cos rt - trY * sin rt, y: trX * sin rt - trY * cos rt }
    , rotate: -rt
    }
