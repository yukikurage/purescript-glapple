module Graphics.Glapple.Hooks.UseUpdate where

import Prelude

import Control.Monad.Reader (ask)
import Effect (Effect)
import Effect.Class (liftEffect)
import Graphics.Glapple.Data.Component (Component)
import Graphics.Glapple.Data.Emitter (addListener_)
import Graphics.Glapple.Hooks.UseFinalize (useFinalize)

useUpdate
  :: forall sprite
   . ({ deltaTime :: Number } -> Effect Unit)
  -> Component sprite Unit
useUpdate listener = do
  { rendererEmitter } <- ask
  remove <- liftEffect $ addListener_ rendererEmitter 0.0 \{ deltaTime } -> do
    listener { deltaTime } *> pure mempty
  useFinalize remove
