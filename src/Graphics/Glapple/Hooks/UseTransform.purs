module Graphics.Glapple.Hooks.UseTransform where

import Prelude

import Control.Monad.Reader (ask)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Ref (read, write)
import Graphics.Canvas (Transform)
import Graphics.Glapple.Data.Component (Component)
import Graphics.Glapple.Util ((|*|))

useLocalTransform
  :: forall sprite
   . Component sprite (Effect Transform /\ (Transform -> Effect Unit))
useLocalTransform = do
  { componentTransform } <- ask
  pure $ (read componentTransform) /\ (\trans -> write trans componentTransform)

useGlobalTransform
  :: forall sprite
   . Component sprite (Effect Transform)
useGlobalTransform = do
  { componentTransform, parentTransform } <- ask
  pure do
    trans0 <- parentTransform
    trans1 <- read componentTransform
    pure $ trans0 |*| trans1
