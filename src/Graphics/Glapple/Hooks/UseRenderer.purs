module Graphics.Glapple.Hooks.UseRenderer where

import Prelude

import Effect.Class (class MonadEffect)
import Graphics.Glapple.Contexts (rendererEmitterContext)
import Graphics.Glapple.Data.Emitter (Emitter, addListener, removeListener)
import Graphics.Glapple.Data.Hooks (Hooks, useContext)
import Graphics.Glapple.Data.Hooks.Qualified as H
import Graphics.Glapple.Data.Picture (Picture)
import Graphics.Glapple.Hooks.UseFinalize (useFinalize)

useRenderer
  :: forall t9 t16 t40
   . Monad t9
  => MonadEffect t9
  => ({ deltaTime :: Number } -> t9 (Picture t16))
  -> Hooks t9
       ( finalizeEmitter :: Emitter Unit Unit t9
       , rendererEmitter :: Emitter { deltaTime :: Number } (Picture t16) t9
       | t40
       )
       ( finalizeEmitter :: Emitter Unit Unit t9
       , rendererEmitter :: Emitter { deltaTime :: Number } (Picture t16) t9
       | t40
       )
       Unit
useRenderer render = H.do
  rendererEmitter <- useContext rendererEmitterContext
  registration <- H.lift $ addListener rendererEmitter render
  useFinalize $ removeListener registration
