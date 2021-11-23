module Graphics.Glapple.Data.GameSpec where

import Prelude

import Graphics.Glapple.Data.Event (Event)
import Graphics.Glapple.Data.GameSpecM (GameSpecM(..))
import Graphics.Glapple.Data.Picture (Picture)
import Graphics.Glapple.GlappleM (GlappleM, getGameState, getGlobalTime, getLocalTime, putGameState)

-- | A pure version of GameSpecM.
newtype GameSpec sprite gameState input = GameSpec
  { initGameState :: gameState
  , render :: { globalTime :: Number, localTime :: Number } -> gameState -> Picture sprite
  , eventHandler :: Event -> gameState -> gameState
  , inputHandler :: input -> gameState -> gameState
  }

-- | Convert GameSpec to GameSpecM.
mkGameSpecM
  :: forall gameState sprite input output
   . GameSpec sprite gameState input
  -> GameSpecM sprite gameState input output
mkGameSpecM
  ( GameSpec
      { initGameState
      , render
      , eventHandler
      , inputHandler
      }
  ) = GameSpecM
  { initGameState: mkInitGameStateM initGameState
  , render: mkRenderM render
  , eventHandler: mkHandlerM eventHandler
  , inputHandler: mkHandlerM inputHandler
  }

mkInitGameStateM :: forall f a. Applicative f => a -> f a
mkInitGameStateM = pure

mkRenderM
  :: forall sprite gameState i output
   . ({ globalTime :: Number, localTime :: Number } -> gameState -> Picture sprite)
  -> GlappleM sprite gameState i output (Picture sprite)
mkRenderM f = do
  gameState <- getGameState
  globalTime <- getGlobalTime
  localTime <- getLocalTime
  pure $ f { globalTime, localTime } gameState

mkHandlerM
  :: forall a sprite gameState i output
   . (a -> gameState -> gameState)
  -> a
  -> GlappleM sprite gameState i output Unit
mkHandlerM f e = do
  gameState <- getGameState
  putGameState $ f e gameState