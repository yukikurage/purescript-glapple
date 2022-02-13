module Graphics.Glapple.Hooks.UseVelocity where

import Prelude

import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Graphics.Glapple.Data.Component (Component)
import Graphics.Glapple.Hooks.UseState (useState)
import Graphics.Glapple.Hooks.UseTransform (useLocalTransform)
import Graphics.Glapple.Hooks.UseUpdate (useUpdate)

useVelocity
  :: forall sprite
   . Component sprite
       ( (Effect { x :: Number, y :: Number }) /\
           ({ x :: Number, y :: Number } -> Effect Unit)
       )
useVelocity = do
  getVelocity /\ setVelocity <- useState { x: 0.0, y: 0.0 }
  getTransform /\ setTransform <- useLocalTransform

  useUpdate \{ deltaTime } -> do
    { x, y } <- getVelocity
    trans@{ m31, m32 } <- getTransform
    let
      x' = m31 + x * deltaTime
      y' = m32 + y * deltaTime
    setTransform $ trans { m31 = x', m32 = y' }

  pure (getVelocity /\ setVelocity)
