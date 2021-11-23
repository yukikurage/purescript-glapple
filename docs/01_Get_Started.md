# Get Started

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

