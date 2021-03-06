module Graphics.Glapple.Hooks.UseDestroy where

import Prelude

import Control.Monad.Reader (ask)
import Effect (Effect)
import Effect.Class (liftEffect)
import Graphics.Glapple.Data.Emitter (emit)
import Graphics.Glapple.Data.Hooks (Hooks)

-- | Destroy a component.
-- | When destroy on top level of component, need to be called last.
useDestroy :: forall sprite. Hooks sprite (Effect Unit)
useDestroy = do
  { finalizeEmitter } <- ask
  let
    destroy = emit finalizeEmitter unit *> pure unit
  pure destroy

-- | Destroy a component on top level of component.
-- | Need to be called last.
useDestroyNow :: forall sprite. Hooks sprite Unit
useDestroyNow = do
  destroy <- useDestroy
  liftEffect destroy
