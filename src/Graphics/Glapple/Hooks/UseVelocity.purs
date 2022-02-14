module Graphics.Glapple.Hooks.UseVelocity where

import Prelude

import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import Graphics.Glapple.Data.Component (Component)
import Graphics.Glapple.Hooks.UseState (useState)
import Graphics.Glapple.Hooks.UseTransform (useTranslate)
import Graphics.Glapple.Hooks.UseUpdate (useUpdate)

useVelocity
  :: forall sprite
   . Component sprite
       ( (Effect { x :: Number, y :: Number }) /\
           ({ x :: Number, y :: Number } -> Effect Unit)
       )
useVelocity = do
  getVelocity /\ setVelocity <- useState { x: 0.0, y: 0.0 }
  getTranslate /\ setTranslate <- useTranslate

  useUpdate \{ deltaTime } -> do
    { x, y } <- getVelocity
    { x: trX, y: trY } <- getTranslate
    let
      trX' = trX + x * deltaTime
      trY' = trY + y * deltaTime
    setTranslate { x: trX', y: trY' }

  pure (getVelocity /\ setVelocity)

useVelocityNow
  :: forall sprite. { x :: Number, y :: Number } -> Component sprite Unit
useVelocityNow vel = do
  _ /\ setVelocity <- useVelocity
  liftEffect $ setVelocity vel
