module Graphics.Glapple.Hooks.UseUpdate where

import Prelude

import Effect.Class (class MonadEffect)
import Graphics.Glapple.Contexts (rendererEmitterContext)
import Graphics.Glapple.Data.Emitter (Emitter, Registration, addListener)
import Graphics.Glapple.Data.Hooks (Hooks, useContext)
import Graphics.Glapple.Data.Hooks.Qualified as H
import Graphics.Glapple.Data.Picture (Picture)

useUpdate
  :: forall m r sprite
   . MonadEffect m
  => ( { deltaTime :: Number
       }
       -> m Unit
     )
  -> Hooks m
       ( rendererEmitter ::
           Emitter
             { deltaTime :: Number
             }
             (Picture sprite)
             m
       | r
       )
       ( rendererEmitter ::
           Emitter
             { deltaTime :: Number
             }
             (Picture sprite)
             m
       | r
       )
       ( Registration
           { deltaTime :: Number
           }
           (Picture sprite)
           m
       )
useUpdate updateHandler = H.do
  rendererEmitter <- useContext rendererEmitterContext
  H.lift $ addListener rendererEmitter \{ deltaTime } -> do
    updateHandler { deltaTime }
    pure mempty
