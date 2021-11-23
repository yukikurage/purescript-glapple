# Sprite System

It is inefficient to do heavy processing every frame (especially `image`). In such a case, the sprite system can be used to read the data first.


First, create a type that represents a sprite. You need to make it an instance of `Ord`.
```purescript
data Sprite = Hoge | Fuga

derive instance Eq Sprite
derive instance Ord Sprite
```

Next, pass an array of sprites and their sources to `runGameM_`.
```purescript
_ <- runGameM_ 60.0 canvas { width: 320.0, height: 320.0 } [ FromImage Hoge "/images/hoge.png", FromPicture Fuga poly1 ] gameSpec
```

In addition, set the first argument of the type constructors `GameM` to Sprite.

```purescript
gameSpec :: GameSpecM Sprite Unit Unit Unit
```

Now you can write an image that is preloaded with the `sprite` function.
```purescript
sprite Hoge
sprite Fuga
```

The code looks like this.

```purescript
main :: Effect Unit
main = do
  canvas <- maybe (throw "Can not find canvas element") pure
    =<< getCanvasElementById "canvas"
  _ <- runGameM_ 60.0 canvas { width: 320.0, height: 320.0 } [ FromImage Hoge "/images/hoge.png", FromPicture Fuga poly1 ] gameSpec
  pure unit

gameSpec :: GameSpecM Sprite Unit Unit Unit
gameSpec = GameSpecM
  { initGameState: pure unit
  , eventHandler: defaultHandler
  , inputHandler: defaultHandler
  , render: pure $ sprite Hoge <-^ sprite Fuga
  }

poly1 = polygon Fill [ 300.0 /\ 200.0, 100.0 /\ 200.0, 0.0 /\ 30.0 ]
  # color (rgb 225 0 225)
```

### SpriteData Type
An array of type `SpriteData` can be passed to `runGameM_`.`SpriteData` is defined as follows.

```
data SpriteData s = FromImage s String | FromPicture s (Picture s)
```

- `FromImage`
Create a sprite from an image path.

- `FromPicture`
Create a sprite from a Picture type.

>Note: If you include `sprite` in the picture specified by `FromPicture`, it will not work stably. Also, it is not possible to sprite a Picture that exceeds the size of the current campus.
