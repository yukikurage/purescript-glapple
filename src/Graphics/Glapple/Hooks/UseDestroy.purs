module Graphics.Glapple.Hooks.UseDestroy where

import Prelude

import Control.Monad.Reader (ask)
import Effect (Effect)
import Graphics.Glapple.Data.Component (Component)
import Graphics.Glapple.Data.Emitter (emit)

useDestroy :: forall sprite. Component sprite (Effect Unit)
useDestroy = do
  { finalizeEmitter } <- ask
  let
    destroy = emit finalizeEmitter unit *> pure unit
  pure destroy
