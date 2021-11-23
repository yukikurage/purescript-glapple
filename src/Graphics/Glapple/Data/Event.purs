module Graphics.Glapple.Data.Event where

import Prelude

data KeyState = KeyDown | KeyUp

derive instance Eq KeyState
derive instance Ord KeyState

data MouseButton = Left | Center | Right

derive instance Eq MouseButton
derive instance Ord MouseButton

data KeyCode = Keyboard String | Mouse MouseButton

derive instance Eq KeyCode
derive instance Ord KeyCode

data Event
  = KeyEvent { keyCode :: KeyCode, keyState :: KeyState }
  | Update { deltaTime :: Number }
  | MouseMove { mouseX :: Number, mouseY :: Number }

derive instance Eq Event
derive instance Ord Event
