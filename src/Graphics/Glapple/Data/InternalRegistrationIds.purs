module Graphics.Glapple.Data.InternalRegistrationIds where

import Prelude

import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect)
import Graphics.Canvas (CanvasImageSource, Context2D)
import Graphics.Glapple.Data.Emitter (RegistrationId, unregister)
import Graphics.Glapple.Data.Event (Event)

type InternalRegistrationIds s i o =
  { inputId :: RegistrationId Effect i
  , renderId :: RegistrationId Aff { context2D :: Context2D, canvasImageSources :: s -> Maybe CanvasImageSource }
  , outputId :: RegistrationId Effect o
  , eventId :: RegistrationId Effect Event
  }

-- | Take InternalRegistrationIds to detach the game from the process.
unregisterGame :: forall s i o m. MonadEffect m => InternalRegistrationIds s i o -> m Unit
unregisterGame { inputId, renderId, outputId, eventId } = do
  unregister inputId
  unregister renderId
  unregister outputId
  unregister eventId