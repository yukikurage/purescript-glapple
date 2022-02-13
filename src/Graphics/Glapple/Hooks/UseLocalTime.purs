module Graphics.Glapple.Hooks.UseLocalTime where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Graphics.Glapple.Data.Component (Component)
import Graphics.Glapple.Util (getNowTime)

useLocalTime
  :: forall sprite
   . Component sprite (Effect Number)
useLocalTime = do
  initTime <- liftEffect getNowTime
  let
    getLocalTime = do
      time <- liftEffect getNowTime
      pure $ time - initTime
  pure getLocalTime
