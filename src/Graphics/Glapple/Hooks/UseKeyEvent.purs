module Graphics.Glapple.Hooks.UseKeyEvent where

import Prelude

import Control.Monad.Reader (ask)
import Effect (Effect)
import Effect.Class (liftEffect)
import Graphics.Glapple.Data.Emitter (addListener_)
import Graphics.Glapple.Data.Hooks (Hooks)
import Graphics.Glapple.Data.KeyEvent (KeyEvent)
import Graphics.Glapple.Hooks.UseFinalize (useFinalize)

-- | A key event hook.
-- | If you want to get state of specific key, use 'useKeyState' hook.
useKeyEvent :: forall sprite. (KeyEvent -> Effect Unit) -> Hooks sprite Unit
useKeyEvent listener = do
  { keyEmitter } <- ask
  remove <- liftEffect $ addListener_ keyEmitter 0.0 listener

  useFinalize remove
