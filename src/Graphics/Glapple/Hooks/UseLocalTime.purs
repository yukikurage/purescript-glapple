module Graphics.Glapple.Hooks.UseLocalTime where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Graphics.Glapple.Data.Hooks (Hooks)
import Graphics.Glapple.Util (getNowTime)

useLocalTime
  :: forall sprite
   . Hooks sprite (Effect Number)
useLocalTime = do
  initTime <- liftEffect getNowTime
  let
    getLocalTime = do
      time <- liftEffect getNowTime
      pure $ time - initTime
  pure getLocalTime
