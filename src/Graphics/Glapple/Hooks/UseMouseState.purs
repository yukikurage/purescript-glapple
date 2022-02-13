module Graphics.Glapple.UseMouseState where

import Prelude

import Control.Monad.Reader (ask)
import Effect (Effect)
import Effect.Ref (read)
import Graphics.Glapple.Data.Component (Component)

useMouseState :: forall sprite. Component sprite (Effect {x :: Number, y :: Number})
useMouseState = do
  { mouseStateRef } <- ask
  let
    getMouseState = read mouseStateRef
  pure getMouseState
