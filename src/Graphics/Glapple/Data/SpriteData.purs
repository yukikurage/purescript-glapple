module Graphics.Glapple.Data.SpriteData where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, error, makeAff)
import Graphics.Canvas (CanvasImageSource, tryLoadImage)
import Graphics.Glapple.Data.Picture (Picture)

tryLoadImageAff :: String -> Aff CanvasImageSource
tryLoadImageAff src = makeAff \f -> do
  tryLoadImage src $ case _ of
    Nothing -> f $ Left $ error "Fail to load image"
    Just img -> f $ Right img
  pure mempty

data SpriteData s = FromImage s String | FromPicture s (Picture s)
