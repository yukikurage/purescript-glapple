# Parent and Child games

You can run another game inside the game, just like running the game with `runGameM`. Use `runChildGameM` for this.

You can also get the `GameId`, `tell`, and `raise` in the same way.

ex)
```purescript
gameSpec :: GameSpecM Sprite { steps :: Int, gameId :: GameId Sprite Int } Unit String
gameSpec = GameSpecM
  { initGameState: do
      let
        outputHandler = \n -> do
          { steps } <- getGameState
          log $ "Parent Game's step is " <> show steps
          log $ "Child Game's step is " <> show n
      gameId <- runChildGameM gameSpec2 outputHandler
      pure { steps: 0, gameId }
  , eventHandler: case _ of
      KeyEvent { keyCode: Keyboard "KeyA", keyState: KeyDown } -> do
        { steps, gameId } <- getGameState
        modifyGameState _ { steps = steps + 1 }
        tell gameId steps
      _ -> pure unit
  , inputHandler: defaultHandler
  , render: do
      { steps, gameId } <- getGameState
      pure $ (renderGame gameId <-^ rect Fill 20.0 20.0)
        # translate (toNumber steps * 10.0) 0.0
  }

gameSpec2 :: GameSpecM Sprite Int Int Int
gameSpec2 = GameSpecM
  { initGameState: pure 10
  , eventHandler: case _ of
      KeyEvent { keyCode: Keyboard "KeyB", keyState: KeyDown } -> do
        modifyGameState (_ - 1)
      _ -> pure unit
  , inputHandler: \n -> do
      steps <- getGameState
      when (n >= 5) do
        raise steps
        destroy
  , render: do
      steps <- getGameState
      pure $ rect Fill 20.0 20.0
        # translate 0.0 (toNumber steps * 10.0)
  }
```
There are a few things worth mentioning.

- First, the parent game has a `GameId` in the game state. This is the place to store the `GameId` of the child game. Parent game run `runChildGameM` in `initGameState` and save the returned `GameId`.

- The `renderGame` function takes a `gameId` and converts it to a `Picture` of the game it points to. Child games are not displayed just by creating them, they need to be explicitly rendered via the `remderGame` function.

- The `destroy` function of the child game deletes its own information from the `gameId` of the parent game. Therefore, after `destroy` is executed, the `gameId` will be the value that does not indicate any game.

The notion of an empty `GameId` is important. In fact, a `GameId` can indicate not only one game, but also zero or more games.

An empty `GameId` can be retrieved with the `emptyGameId` function.

```purescript
gameId <- emptyGameId
```

Use the `runGameWithM` function to add a game to `GameId`.

```purescript
runGameWithM gameId gameSpec outputHandler --Add Game to gameId
```

GameId`, which indicates multiple games, works as follows.

- The `tell` function tells the input to all multiple games.
- The `renderGame` function renders all the multiple games.