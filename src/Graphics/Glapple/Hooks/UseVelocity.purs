module Graphics.Glapple.Hooks.UseVelocity where

import Prelude

import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import Graphics.Glapple.Data.Complex (Complex, (:*))
import Graphics.Glapple.Data.Hooks (Hooks)
import Graphics.Glapple.Hooks.UseState (useState)
import Graphics.Glapple.Hooks.UseTransform (useTranslate)
import Graphics.Glapple.Hooks.UseUpdate (useUpdate)

useVelocity
  :: forall sprite
   . Hooks sprite ((Effect Complex) /\ (Complex -> Effect Unit))
useVelocity = do
  getVelocity /\ setVelocity <- useState $ zero
  getTranslate /\ setTranslate <- useTranslate

  useUpdate \{ deltaTime } -> do
    vel <- getVelocity
    trans <- getTranslate
    let
      trans' = deltaTime :* vel + trans
    setTranslate trans'

  pure (getVelocity /\ setVelocity)

useVelocityNow
  :: forall sprite. Complex -> Hooks sprite Unit
useVelocityNow vel = do
  _ /\ setVelocity <- useVelocity
  liftEffect $ setVelocity vel
