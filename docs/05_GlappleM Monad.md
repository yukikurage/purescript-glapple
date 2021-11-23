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