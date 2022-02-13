module Graphics.Glapple.Hooks.UseInterval where

import Prelude

import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import Graphics.Glapple.Data.Component (Component)
import Graphics.Glapple.Hooks.UseState (useState)
import Graphics.Glapple.Hooks.UseUpdate (useUpdate)
import Graphics.Glapple.Util (getNowTime)

useInterval
  :: forall sprite
   . Number
  -> Effect Unit
  -> Component sprite Unit
useInterval time callback = do
  getPrevTime /\ setPrevTime <- useState =<< liftEffect getNowTime
  useUpdate $ \_ -> do
    prevTime <- getPrevTime
    nt <- liftEffect $ getNowTime
    when (nt - prevTime >= time) do
      callback
      setPrevTime $ nt
