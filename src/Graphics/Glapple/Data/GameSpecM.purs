module Graphics.Glapple.Data.GameSpecM
  ( GameSpecM(..)
  , CanvasSpec
  , defaultRender
  , defaultHandler
  ) where

import Prelude

import Graphics.Glapple.Data.Event (Event)
import Graphics.Glapple.GlappleM (GlappleM)
import Graphics.Glapple.Data.Picture (Picture, empty)

type CanvasSpec =
  { height :: Number
  , width :: Number
  }

-- | runGameM to run the game.
newtype GameSpecM s g i o = GameSpecM
  { initGameState :: GlappleM s g i o g
  , render :: GlappleM s g i o (Picture s)
  , inputHandler :: i -> GlappleM s g i o Unit
  , eventHandler :: Event -> GlappleM s g i o Unit
  }

defaultRender :: forall m s. Applicative m => m (Picture s)
defaultRender = pure empty

defaultHandler :: forall m a. Applicative m => a -> m Unit
defaultHandler = const $ pure unit
