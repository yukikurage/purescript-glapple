module Graphics.Glapple.Data.KeyEvent where

import Prelude

data MouseButton = Left | Middle | Right

derive instance Eq MouseButton
derive instance Ord MouseButton

data KeyCode = Keyboard String | Mouse MouseButton

derive instance Eq KeyCode
derive instance Ord KeyCode

data KeyEvent = KeyDown KeyCode | KeyUp KeyCode

derive instance Eq KeyEvent
derive instance Ord KeyEvent

data KeyState = KeyPressed KeyCode | KeyReleased KeyCode

derive instance Eq KeyState
derive instance Ord KeyState
