module Graphics.Glapple.Util where

import Prelude

import Data.JSDate (getTime, now)
import Effect (Effect)
import Graphics.Canvas (CanvasElement)
import Unsafe.Coerce (unsafeCoerce)
import Web.HTML (HTMLCanvasElement)

foreign import createCanvasElement :: Effect CanvasElement

canvasElementToHTMLCanvasElement :: CanvasElement -> HTMLCanvasElement
canvasElementToHTMLCanvasElement = unsafeCoerce

htmlCanvasElementToCanvasElement :: HTMLCanvasElement -> CanvasElement
htmlCanvasElementToCanvasElement = unsafeCoerce

getNowTime :: Effect Number
getNowTime = (_ / 1000.0) <<< getTime <$> now
