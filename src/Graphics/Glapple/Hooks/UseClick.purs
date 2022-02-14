module Graphics.Glapple.Hooks.UseClick where

import Prelude

import Effect (Effect)
import Graphics.Glapple.Data.Collider (Collider)
import Graphics.Glapple.Data.Component (Component)
import Graphics.Glapple.Data.KeyEvent (KeyCode(..), KeyEvent(..), MouseButton(..))
import Graphics.Glapple.Hooks.UseKeyEvent (useKeyEvent)
import Graphics.Glapple.Hooks.UseRay (useRay)
import Graphics.Glapple.UseMouseState (useMouseState)

-- | オブジェクトがクリックされたときに呼ばれます．
useClick
  :: forall sprite
   . Number
  -> Collider
  -> Effect Unit
  -> Component sprite Unit
useClick layer collider listener = do
  cast <- useRay layer collider
  getMouseState <- useMouseState

  useKeyEvent \key -> do
    mousePos <- getMouseState
    res <- cast mousePos
    when (res && key == KeyDown (Mouse Left)) $ listener
