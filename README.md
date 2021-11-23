<!-- omit in toc -->
# purescript-glapple
- [Installation](#installation)
- [Documentation](#documentation)
  - [Get Started](#get-started)
  - [Picture Type](#picture-type)
  - [Sprite System](#sprite-system)
    - [SpriteData Type](#spritedata-type)
  - [GameState](#gamestate)
  - [EventHandler](#eventhandler)
  - [`tell` and InputHandler](#tell-and-inputhandler)
  - [`raise` and OutputHandler](#raise-and-outputhandler)
  - [GlappleM Monad](#glapplem-monad)
  - [Parent and child games](#parent-and-child-games)
## Installation
in `packages.dhall`
```dhall
...
let upstream = ...
in  upstream
  with glapple =
    { dependencies =
        [ "aff"
        , "arrays"
        , "canvas"
        , "colors"
        , "console"
        , "datetime"
        , "effect"
        , "either"
        , "foldable-traversable"
        , "integers"
        , "math"
        , "maybe"
        , "now"
        , "ordered-collections"
        , "prelude"
        , "psci-support"
        , "refs"
        , "safely"
        , "tailrec"
        , "transformers"
        , "tuples"
        , "unsafe-coerce"
        , "web-events"
        , "web-html"
        , "web-uievents"
        ]
    , repo =
        "https://github.com/yukikurage/purescript-glapple"
    , version =
        "v1.0.0"
    }
...
```
and run

`spago install glapple`

## Documentation

### Get Started

`index.html`
```html
<!DOCTYPE html>
<html>
    <head>
        <title>Glapple Test</title>
        <script async="" type="text/javascript" src="index.js"></script>
    </head>
    <body>
        <canvas id = "canvas"></canvas>
    </body>
</html>
```

`src/Main.purs`
```purescript
module Main where

import Prelude

import Data.Maybe (maybe)
import Effect (Effect)
import Effect.Exception (throw)
import Graphics.Canvas (TextBaseline(..), getCanvasElementById)
import Graphics.Glapple (GameSpecM(..), defaultHandler, runGameM_)
import Graphics.Glapple.Data.Picture (Shape(..), text, textBaseLine)

main :: Effect Unit
main = do
  canvas <- maybe (throw "Can not find canvas element") pure
    =<< getCanvasElementById "canvas"
  _ <- runGameM_ 60.0 canvas { width: 320.0, height: 320.0 } [] gameSpec
  pure unit

gameSpec :: GameSpecM Unit Unit Unit Unit
gameSpec = GameSpecM
  { initGameState: pure unit
  , eventHandler: defaultHandler
  , inputHandler: defaultHandler
  , render: pure $ textBaseLine BaselineHanging $ text Fill "Hello world"
  }
```

Bundle it into `index.js` using an appropriate tool. Then you get:
![image](https://user-images.githubusercontent.com/55534323/143014373-d16b75f0-4aa0-4741-82c5-736e6f41a4dd.png)

### Picture Type

```purescript
pure $ textBaseLine BaselineHanging $ text Fill "Hello world"
```

Here, text is drawn and `textBaseLine` is set to the top of the text. If you don't do this, the text will stick out of the screen.

You can change the font of the text.

```purescript
pure $ font testFont $ textBaseLine BaselineHanging $ text Fill "Hello world"

...

testFont :: Font
testFont = Font
  { fontFamily: Monospace
  , fontHeight: 40
  , fontSize: 40
  , fontWeight: Bold
  , fontStyle: FontStyleNormal
  }
```
(I plan to simplify this setting in the future.)


You can also move it and rotate it.

```purescript
pure $ translate 50.0 50.0 $ rotate (0.5 * pi) $ font testFont $ textBaseLine BaselineHanging $ text Fill "Hello world"
```
>Note: The center of rotation is always (0, 0).

It will be easier to understand if you write it like a procedure using `#`.

```purescript
pure $ text Fill "Hello world"
  # textBaseLine BaselineHanging
  # font testFont
  # rotate (0.5 * pi)
  # translate 50.0 50.0
```

- Draws a rectangle with the `rect` function.

```purescript
pure $ rect Fill 40.0 50.0
  # translate 50.0 50.0
```

- Draw an arc with the `arc` function.

```purescript
pure $ arc { start: pi / 4.0, angle: pi / 2.0, radius: 30.0 }
  # translate 50.0 50.0
```
>Note: Draws clockwise from the angle specified by `start` by the angle specified by `angle`.
If `angle` is negative, it will be drawn counter-clockwise.
If `angle` is not in the range -2π to 2π, then 4π is added or subtracted to bring it into the range.

- Draw a polygon with the `polygon` function.

```purescript
pure $ polygon Fill [ 300.0 /\ 200.0, 100.0 /\ 200.0, 0.0 /\ 30.0 ]
```

- Change `Fill` to `Stroke` to draw a border.

```purescript
pure $ polygon Stroke [ 300.0 /\ 200.0, 100.0 /\ 200.0, 0.0 /\ 30.0 ]
pure $ rect Stroke 40.0 50.0
```

- Change the lineWidth of a line with `lineWidth`.

```purescript
pure $ polygon Stroke [ 300.0 /\ 200.0, 100.0 /\ 200.0, 0.0 /\ 30.0 ]
  # lineWidth 10.0
```

>Note:For functions that cannot be combined, such as `lineWidth`, the innermost one will be applied.


```purescript
pure $ polygon Stroke [ 300.0 /\ 200.0, 100.0 /\ 200.0, 0.0 /\ 30.0 ]
  # lineWidth 10.0
  # lineWidth 20.0 --invalid
```

- You can change the color of the image with `color`.

```purescript
pure $ polygon Fill [ 300.0 /\ 200.0, 100.0 /\ 200.0, 0.0 /\ 30.0 ]
  # color (rgb 225 0 225)
```

- You can use `<-^` to pile up a `Picture`.

```purescript
pure $ poly1 <-^ poly2

...

poly1 = polygon Fill [ 300.0 /\ 200.0, 100.0 /\ 200.0, 0.0 /\ 30.0 ]
  # color (rgb 225 0 225)

poly2 = polygon Fill [ 100.0 /\ 100.0, 200.0 /\ 100.0, 20.0 /\ 30.0 ]
  # color (rgb 0 0 225)
```

- You can use `image` to draw an image.

```purescript
pure $ image "/images/hoge.png"
```

There are many more functions, but their documents are under preparation.

### Sprite System
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

#### SpriteData Type
An array of type `SpriteData` can be passed to `runGameM_`.`SpriteData` is defined as follows.

```
data SpriteData s = FromImage s String | FromPicture s (Picture s)
```

- `FromImage`
Create a sprite from an image path.

- `FromPicture`
Create a sprite from a Picture type.

>Note:If you include `sprite` in the picture specified by `FromPicture`, it will not work stably. Also, it is not possible to sprite a Picture that exceeds the size of the current campus.

### GameState
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

### EventHandler
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
https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/code
`MouseButton` indicates which mouse button was pressed. Literally. `Left` is the left button, `RIght` is the right button, and `Center` is the wheel button.
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
You can also use the `putGameState` function to replace the game state.

### `tell` and InputHandler
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

### `raise` and OutputHandler
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

### GlappleM Monad
There is no documentation yet.

### Parent and child games
There is no documentation yet.