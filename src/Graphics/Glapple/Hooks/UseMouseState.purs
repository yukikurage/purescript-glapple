module Graphics.Glapple.UseMouseState where

import Prelude

import Control.Monad.Reader (ask)
import Effect (Effect)
import Effect.Ref (read)
import Graphics.Glapple.Data.Complex (Complex)
import Graphics.Glapple.Data.Hooks (Hooks)

useMouseState
  :: forall sprite. Hooks sprite (Effect Complex)
useMouseState = do
  { mouseStateRef } <- ask
  let
    getMouseState = read mouseStateRef
  pure getMouseState
