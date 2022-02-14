module Graphics.Glapple.Hooks.UseRay where

import Prelude

import Control.Monad.Reader (ask)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import Graphics.Glapple.Data.Collider (Collider(..), isCollidePosition)
import Graphics.Glapple.Data.Complex (Complex)
import Graphics.Glapple.Data.Component (Component)
import Graphics.Glapple.Data.Emitter (addListener, emit)
import Graphics.Glapple.Hooks.UseFinalize (useFinalize)
import Graphics.Glapple.Hooks.UseState (useState)
import Graphics.Glapple.Hooks.UseTransform (useGlobalTransform)

-- | useRay は，キャンバスの手前側から光線を飛ばし，光が当たったか，当たっていないかを判定．
-- | クリック判定などに使用する．
useRay
  :: forall sprite
   . Number
  -> Collider
  -> Component sprite (Complex -> Effect Boolean)
useRay layer collider = do
  getTransform <- useGlobalTransform

  { rayEmitter } <- ask
  getIsCollide /\ setIsCollide <- useState false

  remover <- liftEffect $ addListener rayEmitter (-layer) $ \comp prevent ->
    do
      trans <- getTransform
      if isCollidePosition comp (ColliderTransform trans collider) then do
        setIsCollide true
        prevent
      else do
        setIsCollide false

  useFinalize remover

  pure \comp -> do
    _ <- emit rayEmitter comp
    getIsCollide
