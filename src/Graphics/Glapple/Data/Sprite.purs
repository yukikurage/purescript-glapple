module Graphics.Glapple.Data.Sprite
  ( Sprite(..)
  , loadSprites
  , tryLoadImageAff
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Map (Map, fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Traversable (for)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff, error, makeAff)
import Graphics.Canvas (CanvasImageSource, tryLoadImage)

data Sprite s = Source s String

tryLoadImageAff :: String -> Aff CanvasImageSource
tryLoadImageAff src = makeAff \f -> do
  tryLoadImage src $ case _ of
    Nothing -> f $ Left $ error "Fail to load image"
    Just img -> f $ Right img
  pure mempty

loadSprites
  :: forall s
   . Ord s
  => Array (Sprite s)
  -> Aff (Map s CanvasImageSource)
loadSprites xs = do
  ys <- for xs \(Source s src) -> do
    img <- tryLoadImageAff src
    pure $ s /\ img
  pure $ fromFoldable ys
