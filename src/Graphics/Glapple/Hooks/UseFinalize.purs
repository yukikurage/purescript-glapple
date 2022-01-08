module Graphics.Glapple.Hooks.UseFinalize where

import Prelude

import Effect.Class (class MonadEffect)
import Graphics.Glapple.Contexts (finalizeEmitterContext)
import Graphics.Glapple.Data.Emitter (Emitter, addListener)
import Graphics.Glapple.Data.Hooks (Hooks, useContext)
import Graphics.Glapple.Data.Hooks.Qualified as H

useFinalize
  :: forall m r
   . MonadEffect m
  => m Unit
  -> Hooks m
       (finalizeEmitter :: Emitter Unit Unit m | r)
       (finalizeEmitter :: Emitter Unit Unit m | r)
       Unit
useFinalize handler = H.do
  finalizeEmitter <- useContext finalizeEmitterContext
  _ <- H.lift $ addListener finalizeEmitter \_ -> handler
  H.pure unit
