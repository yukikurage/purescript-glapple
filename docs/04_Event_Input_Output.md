# Event, Input, and Output

## GameState

A game can have an internal state. The second argument of `GameSpecM` is the type of the internal state of the game.`initialGameState` can be used to specify the initial game state.
```purescript
gameSpec :: GameSpecM Sprite Int Unit Unit
gameSpec = GameSpecM
  { initGameState: pure 0
  , eventHandler: defaultHandler
  , inputHandler: defaultHandler
  , render: pure $ rect 20.0 40.0
      # transform 50.0 50.0
  }
```

## Event
You can handle events with `eventHandler`. Events are represented by the type `Event`. The definition of `Event` type is as follows.
```purescript
data KeyState = KeyDown | KeyUp

data MouseButton = Left | Center | Right

data KeyCode = Keyboard String | Mouse MouseButton

data Event
  = KeyEvent { keyCode :: KeyCode, keyState :: KeyState }
  | Update { deltaTime :: Number }
  | MouseMove { mouseX :: Number, mouseY :: Number }
```

- `KeyEvent`

Keyboard and mouse button events. The `KeyCode` tells which key was pressed. The `String` of `KeyBoard String` stores the value that can be obtained with `KeyboardEvent.code` in JavaScript.

>https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/code

`MouseButton` indicates which mouse button was pressed. Literally, `Left` is the left button, `Right` is the right button, and `Center` is the wheel button.
The `KeyState` stores whether the button is pressed (`KeyDown`) or released (`KeyUp`).

- `Update`

The `Update` event will be executed every frame. The `deltaTime` is the time elapsed since the last `Update` event.

- `MouseMove`

The `MouseMove` event is called when the mouse is moved. It stores the position in `mouseX` and `mouseY`.

example: Each time `A` on the keyboard is pressed, the image is moved 10 pixels to the right.

```purescript
gameSpec :: GameSpecM Sprite Int Unit Unit
gameSpec = GameSpecM
  { initGameState: pure 0
  , eventHandler: case _ of
    KeyEvent {keyCode: Keyboard "KeyA", keyState: KeyDown} -> modifyGameState (_ + 1)
    _ -> pure unit
  , inputHandler: defaultHandler
  , render: do
    steps <- getGameState
    pure $ poly1
      # translate (toNumber steps * 10.0) 0.0
  }
```

`modifyGameState` and `getGameState` are functions to modify/reference the game state.
You can also use the `putGameState` function to replace the game state. Detailed explanation will be given in `05_GlappleM_Monad`.


## `tell` and InputHandler
You can also give instructions to the game from outside the game. This is made possible by the `tell` function.

The third argument of `GameSpecM` is the type used for input.

This example, for an external input of type `Int`, the current state is overwritten with the input.

```purescript
gameSpec :: GameSpecM Sprite Int Int Unit
gameSpec = GameSpecM
  { initGameState: pure 0
  , eventHandler: case _ of
      KeyEvent { keyCode: Keyboard "KeyA", keyState: KeyDown } -> modifyGameState (_ + 1)
      _ -> pure unit
  , inputHandler: \n -> putGameState n --inputHandler
  , render: do
      steps <- getGameState
      pure $ poly1
        # translate (toNumber steps * 10.0) 0.0
  }
```

`runGameM_` returns the `GameId` as soon as the game is executed.

```purescript
gameId <- runGameM_ 60.0 canvas ...
```

The `tell` function can then tell the game what to do.

```
tell gameId 10
```

## `raise` and OutputHandler
On the other hand, there are times when you want to give orders from inside the game to the outside. This is made possible by the `raise` function.

The fourth argument of `GameSpecM` indicates the type to be used for output.

In this example, the state of the game when the keyboard is pressed is passed to the outside world as a `String` using the `show` function.

```purescript
gameSpec :: GameSpecM Sprite Int Int String
gameSpec = GameSpecM
  { initGameState: pure 0
  , eventHandler: case _ of
      KeyEvent { keyCode: Keyboard "KeyA", keyState: KeyDown } -> do
        steps <- getGameState
        raise $ show steps -- send output to the outside.
        modifyGameState (_ + 1)
      _ -> pure unit
  , inputHandler: defaultHandler
  , render: do
      steps <- getGameState
      pure $ poly1
        # translate (toNumber steps * 10.0) 0.0
  }
```

If you change `runGameM_` function to `runGameM` function, you can take outputHandler as the last argument.

In the following example, the output from the game is output to the console

```purescript
let
  outputHandler = \o -> log o
_ <- runGameM 60.0 canvas { width: 320.0, height: 320.0 } [] gameSpec outputHandler
```
