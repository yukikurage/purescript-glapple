module Graphics.Glapple.Data.GameId where

import Prelude

import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect, liftEffect)
import Graphics.Canvas (CanvasImageSource, Context2D)
import Graphics.Glapple.Data.Emitter (EmitterId, emitterSize, fire, newEmitter)
import Graphics.Glapple.Data.Picture (Picture, absorb', empty)

-- | Used for input from outside the game.
data GameId s i =
  GameId
    { inputEmitter :: EmitterId Effect i
    , renderEmitter ::
        EmitterId Aff
          { canvasImageSources :: s -> Maybe CanvasImageSource
          , context2D :: Context2D
          }
    }

-- | Tell input to the game.
tell
  :: forall s i m
   . MonadEffect m
  => GameId s i
  -> i
  -> m Unit
tell (GameId { inputEmitter }) input = liftEffect $ fire inputEmitter $ input

-- | Draw the game.
renderGame
  :: forall s i
   . GameId s i
  -> Picture s
renderGame (GameId { renderEmitter }) = absorb' \context2D canvasImageSources ->
  do
    fire renderEmitter { context2D, canvasImageSources }
    pure empty

-- | Empty GameId.
emptyGameId :: forall m s i. Bind m => MonadEffect m => m (GameId s i)
emptyGameId = do
  inputEmitter <- newEmitter
  renderEmitter <- newEmitter
  pure $ GameId { inputEmitter, renderEmitter }

-- | Returns True if GameId is empty.
null :: forall s i m. MonadEffect m => GameId s i -> m Boolean
null (GameId { inputEmitter, renderEmitter }) = do
  inputS <- emitterSize inputEmitter
  renderS <- emitterSize renderEmitter

  pure $ inputS == 0 && renderS == 0