module Graphics.Glapple.Hooks.UseClick where

import Prelude

import Effect (Effect)
import Graphics.Glapple.Data.Collider (Collider)
import Graphics.Glapple.Data.Hooks (Hooks)
import Graphics.Glapple.Data.KeyEvent (KeyCode(..), KeyEvent(..), MouseButton(..))
import Graphics.Glapple.Hooks.UseHover (useHover)
import Graphics.Glapple.Hooks.UseKeyEvent (useKeyEvent)

-- | オブジェクトがクリックされたときに呼ばれます．
useClick
  :: forall sprite
   . Number
  -> Collider
  -> Effect Unit
  -> Hooks sprite Unit
useClick layer collider listener = do
  getIsHover <- useHover layer collider

  useKeyEvent \key -> do
    res <- getIsHover
    when (res && key == KeyDown (Mouse Left)) $ listener
