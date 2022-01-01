module Graphics.Glapple.Data.SpriteData where

import Prelude

import Data.Either (Either(..))
import Data.HashMap (HashMap, fromArray)
import Data.Hashable (class Hashable)
import Data.Maybe (Maybe(..))
import Data.Traversable (for)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Aff, error, makeAff)
import Graphics.Canvas (CanvasImageSource, tryLoadImage)

tryLoadImageAff :: String -> Aff CanvasImageSource
tryLoadImageAff src = makeAff \f -> do
  tryLoadImage src $ case _ of
    Nothing -> f $ Left $ error "Fail to load image"
    Just img -> f $ Right img
  pure mempty

loadImages
  :: forall s
   . Hashable s
  => Array (s /\ String)
  -> Aff (HashMap s CanvasImageSource)
loadImages xs = do
  ys <- for xs \(s /\ src) -> do
    img <- tryLoadImageAff src
    pure $ s /\ img
  pure $ fromArray ys