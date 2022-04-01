module Graphics.Glapple.Hooks.UseHover where

import Prelude

import Control.Monad.Reader (ask)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import Graphics.Glapple.Data.Collider (Collider(..), isCollidePosition)
import Graphics.Glapple.Data.Emitter (addListener)
import Graphics.Glapple.Data.Hooks (Hooks)
import Graphics.Glapple.Hooks.UseFinalize (useFinalize)
import Graphics.Glapple.Hooks.UseState (useState)
import Graphics.Glapple.Hooks.UseTransform (useGlobalTransform)

-- | useHover は，マウスがホバーしているかどうかを判定する．
-- | クリック判定などに使用する．
-- | 第1引数(layer)が小さいほど手前にあり，優先される．
-- | layerが同じ場合は順序は保障されない．
useHover
  :: forall sprite
   . Number
  -> Collider
  -> Hooks sprite (Effect Boolean)
useHover layer collider = do
  getTransform <- useGlobalTransform

  { hoverEmitter } <- ask
  getIsHover /\ setIsHover <- useState false

  remover <- liftEffect $ addListener hoverEmitter (-layer) $ \comp prevent ->
    do
      trans <- getTransform
      if isCollidePosition comp (ColliderTransform trans collider) then do
        setIsHover true
        prevent
      else do
        setIsHover false

  useFinalize remover

  pure getIsHover
