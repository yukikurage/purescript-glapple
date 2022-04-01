module Graphics.Glapple.Runner where

import Prelude

import Control.Monad.Rec.Class (forever)
import Data.Int (toNumber)
import Data.Map (lookup)
import Data.Maybe (Maybe(..))
import Data.Set as S
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (delay, launchAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Ref (modify_, new, read, write)
import Graphics.Canvas (CanvasElement, canvasElementToImageSource, clearRect, drawImage, getContext2D, setCanvasHeight, setCanvasWidth)
import Graphics.Glapple.Data.Complex (Complex, complex)
import Graphics.Glapple.Data.Component (Component(..))
import Graphics.Glapple.Data.Emitter (emit, newEmitter)
import Graphics.Glapple.Data.Hooks (runHooks)
import Graphics.Glapple.Data.KeyEvent (KeyCode(..), KeyEvent(..), MouseButton(..))
import Graphics.Glapple.Data.Sprite (Sprite, loadSprites)
import Graphics.Glapple.Data.Transform (unitTransform)
import Graphics.Glapple.Util (createCanvasElement, getNowTime)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Element (clientHeight, clientWidth)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (HTMLCanvasElement, window)
import Web.HTML.HTMLCanvasElement as HTMLCanvasElement
import Web.HTML.HTMLElement (getBoundingClientRect)
import Web.HTML.Window as Window
import Web.UIEvent.KeyboardEvent (code, repeat)
import Web.UIEvent.KeyboardEvent as KeyboardEvent
import Web.UIEvent.KeyboardEvent.EventTypes (keydown, keyup)
import Web.UIEvent.MouseEvent as MouseEvent
import Web.UIEvent.MouseEvent.EventTypes (mousedown, mousemove, mouseup)

runGame
  :: forall props a sprite
   . Ord sprite
  => { canvas :: CanvasElement
     , fps :: Number
     , height :: Number
     , sprites :: Array (Sprite sprite)
     , width :: Number
     , initMousePosition :: Complex
     }
  -> Component props sprite a
  -> props
  -> Effect Unit
runGame
  { canvas, fps, height, width, sprites, initMousePosition }
  (Component component)
  props =
  do
    rendererEmitter <- newEmitter
    hoverEmitter <- newEmitter
    finalizeEmitter <- newEmitter
    keyEmitter <- newEmitter
    keyStateRef <- liftEffect $ new S.empty
    mouseStateRef <- liftEffect $ new initMousePosition
    componentTransform <- liftEffect $ new unitTransform

    -- 初期化
    _ <-
      runHooks
        { rendererEmitter
        , finalizeEmitter
        , hoverEmitter
        , keyEmitter
        , keyStateRef
        , mouseStateRef
        , componentTransform
        , parentTransform: pure unitTransform
        } $
        component props

    -- 裏画面の生成
    subCanvas <- createCanvasElement
    ctx <- getContext2D canvas
    subCtx <- getContext2D subCanvas
    liftEffect do
      setCanvasHeight canvas height
      setCanvasWidth canvas width
      setCanvasHeight subCanvas height
      setCanvasWidth subCanvas width

    -- Windowを取得
    win <- Window.toEventTarget <$> window

    -- Web EventでEmitterを発火させる
    keyDownHandler <- eventListener \e -> case KeyboardEvent.fromEvent e of
      Just keyE | not (repeat keyE) -> do
        _ <- emit keyEmitter $ KeyDown $ Keyboard $ code keyE
        liftEffect $ modify_ (S.insert $ Keyboard $ code keyE) keyStateRef
      _ -> pure unit
    addEventListener keydown keyDownHandler false $ win

    keyUpHandler <- eventListener \e -> case KeyboardEvent.fromEvent e of
      Just keyE | not (repeat keyE) -> do
        _ <- emit keyEmitter $ KeyUp $ Keyboard $ code keyE
        modify_ (S.delete $ Keyboard $ code keyE) keyStateRef
      _ -> pure unit
    addEventListener keyup keyUpHandler false $ win

    let
      htmlCanvasElement = unsafeCoerce canvas :: HTMLCanvasElement
      element = HTMLCanvasElement.toElement htmlCanvasElement

    mouseDownHandler <- eventListener \e -> case MouseEvent.fromEvent e of
      Just mouseE -> do
        let
          button = case MouseEvent.button mouseE of
            0 -> Just Left
            1 -> Just Middle
            2 -> Just Right
            _ -> Nothing
        case button of
          Just b -> do
            emit keyEmitter $ KeyDown $ Mouse b
            modify_ (S.insert $ Mouse $ b) keyStateRef
          Nothing -> pure unit
      _ -> pure unit
    addEventListener mousedown mouseDownHandler false win

    mouseUpHandler <- eventListener \e -> case MouseEvent.fromEvent e of
      Just mouseE -> do
        let
          button = case MouseEvent.button mouseE of
            0 -> Just Left
            1 -> Just Middle
            2 -> Just Right
            _ -> Nothing
        case button of
          Just b -> do
            emit keyEmitter $ KeyUp $ Mouse b
            modify_ (S.delete $ Mouse b) keyStateRef
          Nothing -> pure unit
      _ -> pure unit
    addEventListener mouseup mouseUpHandler false win

    mouseMoveHandler <- eventListener \e -> case MouseEvent.fromEvent e of
      Just mouseE -> do
        { left, top } <- getBoundingClientRect $ HTMLCanvasElement.toHTMLElement
          htmlCanvasElement
        let
          mouseX = toNumber (MouseEvent.clientX mouseE) - left
          mouseY = toNumber (MouseEvent.clientY mouseE) - top
        h <- clientHeight element
        w <- clientWidth element
        let
          scaleH = h / height
          scaleW = w / width
          x = mouseX / scaleW
          y = mouseY / scaleH
        write (complex x y) mouseStateRef
      _ -> pure unit
    addEventListener mousemove mouseMoveHandler false win

    launchAff_ do
      -- スプライトの読み込み
      spriteMap <- loadSprites sprites
      let
        images = \key -> lookup key spriteMap

      deltaTimeRef <- liftEffect $ new =<< getNowTime

      -- メインループ
      forever do
        procStart <- liftEffect $ getNowTime

        prevTime <- liftEffect $ read deltaTimeRef
        liftEffect $ write procStart deltaTimeRef

        liftEffect $ clearRect subCtx { x: 0.0, y: 0.0, height, width }
        liftEffect $ emit rendererEmitter $
          { deltaTime: procStart - prevTime
          , ctx: subCtx
          , canvasImageSources: images
          }
        liftEffect $ clearRect ctx { x: 0.0, y: 0.0, height, width }
        liftEffect $ drawImage ctx (canvasElementToImageSource subCanvas) 0.0
          0.0

        -- Hover 判定
        mousePos <- liftEffect $ read mouseStateRef
        liftEffect $ emit hoverEmitter mousePos

        -- fps調整
        procEnd <- liftEffect $ getNowTime
        liftAff $ delay $ Milliseconds $ max 0.0 $ 1000.0 *
          (1.0 / fps - procEnd + procStart)
