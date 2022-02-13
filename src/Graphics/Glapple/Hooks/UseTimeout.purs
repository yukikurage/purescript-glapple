module Graphics.Glapple.Hooks.UseTimeout where

import Prelude

import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Graphics.Glapple.Data.Component (Component)
import Graphics.Glapple.Hooks.UseLocalTime (useLocalTime)
import Graphics.Glapple.Hooks.UseState (useState)
import Graphics.Glapple.Hooks.UseUpdate (useUpdate)

useTimeout
  :: forall sprite
   . Number
  -> Effect Unit
  -> Component sprite Unit
useTimeout time callback = do
  getTime <- useLocalTime
  getFired /\ setFired <- useState false

  useUpdate $ \_ -> do
    currentTime <- getTime
    fired <- getFired
    when (currentTime >= time && not fired) do
      callback
      setFired true
