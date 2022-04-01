module Graphics.Glapple.Hooks.UseRenderer where

import Prelude

import Control.Monad.Reader (ask)
import Effect (Effect)
import Effect.Class (liftEffect)
import Graphics.Glapple.Data.Emitter (addListener_)
import Graphics.Glapple.Data.Hooks (Hooks)
import Graphics.Glapple.Data.Picture (Picture, drawPicture, transform)
import Graphics.Glapple.Hooks.UseFinalize (useFinalize)
import Graphics.Glapple.Hooks.UseTransform (useGlobalTransform)

-- | Rendererを追加します．
-- | 第1引数(layer)の値が小さいほど手前に描画されます．
useRenderer
  :: forall sprite
   . Number
  -> Effect (Picture sprite)
  -> Hooks sprite Unit
useRenderer layer picture = do
  getTransform <- useGlobalTransform

  { rendererEmitter } <- ask
  remove <- liftEffect $ addListener_ rendererEmitter layer
    \{ canvasImageSources, ctx } -> do
      trans <- getTransform
      pic <- picture
      drawPicture ctx canvasImageSources $ transform trans pic

  useFinalize remove
