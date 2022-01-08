module Graphics.Glapple.Contexts
  ( finalizeEmitterContext
  , rendererEmitterContext
  ) where

import Prelude

import Graphics.Glapple.Data.Context (Context(..))
import Graphics.Glapple.Data.Emitter (Emitter)
import Graphics.Glapple.Data.Picture (Picture)

rendererEmitterContext
  :: forall sprite m
   . Context "rendererEmitter"
       (Emitter { deltaTime :: Number } (Picture sprite) m)
rendererEmitterContext = Context

finalizeEmitterContext
  :: forall m
   . Context "finalizeEmitter"
       (Emitter Unit Unit m)
finalizeEmitterContext = Context
