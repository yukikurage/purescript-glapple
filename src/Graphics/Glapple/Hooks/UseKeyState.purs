module Graphics.Glapple.UseKeyState where

import Prelude

import Control.Monad.Reader (ask)
import Data.Set (member)
import Effect (Effect)
import Effect.Ref (read)
import Graphics.Glapple.Data.Hooks (Hooks)
import Graphics.Glapple.Data.KeyEvent (KeyCode)

useKeyState :: forall sprite. Hooks sprite (KeyCode -> Effect Boolean)

useKeyState = do
  { keyStateRef } <- ask
  let
    getKeyState key = do
      keyState <- read keyStateRef
      pure $ member key keyState
  pure getKeyState
