# GlappleM Monad

`eventHandler`, `inputHandler`, and `render` are represented using the `GlappleM` Monad.

`GlappleM` Monad is an instance of `MonadEffect`, so it can perform operations on `Effect`.

ex)Outputs the current game state to the console each time a key is pressed.

```purescript
gameSpec :: GameSpecM Sprite Int Int String
gameSpec = GameSpecM
  { initGameState: pure 0
  , eventHandler: case _ of
      KeyEvent { keyCode: Keyboard "KeyA", keyState: KeyDown } -> do
        steps <- getGameState
        liftEffect $ logShow steps
        modifyGameState (_ + 1)
      _ -> pure unit
  , inputHandler: defaultHandler
  , render: do
      steps <- getGameState
      pure $ poly1
        # translate (toNumber steps * 10.0) 0.0
  }
```

### Available functions
- `getGameState :: forall s g i o. GlappleM s g i o g`

Get current game state. If the game state does not exist before initialization, the process will stop there.

- `putGameState :: forall s g i o. g -> GlappleM s g i o Unit`
- 
Put game state.

- `modifyGameState :: forall s g i o. (g -> g) -> GlappleM s g i o Unit`

Modify function to game state.

- `getGlobalTime :: forall s g i o. GlappleM s g i o Number`

Get the time since root game runs.

- `getLocalTime :: forall s g i o. GlappleM s g i o Number`

Get the time since current game runs.

- `raise :: forall s g i o. o -> GlappleM s g i o Unit

Raise output to parent game.

- `getKeyState :: forall s g i o. KeyCode -> GlappleM s g i o Boolean`

Gets the current key press state.

ex)
```
frag <- getKeyState $ Keyboard "KeyA"
--  frag == true when "A" key pressed.
```

- `getMousePosition :: forall s g i o. GlappleM s g i o (Maybe { mouseX :: Number, mouseY :: Number })`

Gets the current mouse position. Returns `Nothing` if the mouse position cannot be obtained because the user has not moved the mouse yet.

- break :: forall s g i o a. GlappleM s g i o a

Break the current process. Current process means `inputHandler`, `outputHandler`, and `render`. 

- toEffect :: forall s g i o a x. (x -> GlappleM s g i o a) -> GlappleM s g i o (x -> Effect (Maybe a))

Express the process of GlappleM by Effect.

ex)
```
f <- toEffect \i -> putGameState i
-- Then type of f is `g -> Effect (Maybe Unit)`
```

