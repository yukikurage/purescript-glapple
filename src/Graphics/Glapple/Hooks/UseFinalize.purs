module Graphics.Glapple.Hooks.UseFinalize where

import Prelude

import Control.Monad.Reader (ask)
import Effect (Effect)
import Effect.Class (liftEffect)
import Graphics.Glapple.Data.Component (Component)
import Graphics.Glapple.Data.Emitter (addListener_)

useFinalize
  :: forall sprite. Effect Unit -> Component sprite Unit
useFinalize listener = do
  { finalizeEmitter } <- ask
  _ <- liftEffect $ addListener_ finalizeEmitter 0.0 \_ -> listener
  pure unit
