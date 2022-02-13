module Graphics.Glapple.Hooks.UseState where

import Prelude

import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref (new, read, write)
import Graphics.Glapple.Data.Component (Component)

useState
  :: forall sprite a
   . a
  -> Component sprite (Effect a /\ (a -> Effect Unit))
useState a = do
  ref <- liftEffect $ new a
  pure $ (liftEffect $ read ref) /\ (liftEffect <<< flip write ref)
