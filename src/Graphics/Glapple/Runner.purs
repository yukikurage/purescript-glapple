module Graphics.Glapple.Runner (run) where

import Prelude

import Control.Monad.Rec.Class (class MonadRec, forever)
import Data.Array (fold)
import Data.HashMap (lookup)
import Data.Hashable (class Hashable)
import Data.Time (diff)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Now (nowTime)
import Effect.Ref (new, read, write)
import Graphics.Canvas (CanvasElement, canvasElementToImageSource, clearRect, drawImage, getCanvasHeight, getCanvasWidth, getContext2D, setCanvasHeight, setCanvasWidth)
import Graphics.Glapple.Contexts (finalizeEmitterContext, rendererEmitterContext)
import Graphics.Glapple.Data.Emitter (Emitter, emit, newEmitter)
import Graphics.Glapple.Data.Hooks (Hooks, addContext, runHooks)
import Graphics.Glapple.Data.Hooks.Qualified as H
import Graphics.Glapple.Data.Picture (Picture, drawPicture)
import Graphics.Glapple.Data.Sprite (Sprite, loadSprites)

foreign import createCanvasElement :: Effect CanvasElement

run
  :: forall m r never sprite
   . Applicative m
  => MonadAff m
  => Hashable sprite
  => MonadRec m
  => Number
  -> Array (Sprite sprite)
  -> CanvasElement
  -> Hooks m
       ( finalizeEmitter :: Emitter Unit Unit m
       , rendererEmitter :: Emitter { deltaTime :: Number } (Picture sprite) m
       )
       r
       Unit
  -> m never

run fps sprites canvas game = runHooks H.do
  rendererEmitter <- H.lift $ newEmitter
  finalizeEmitter <- H.lift $ newEmitter
  addContext rendererEmitterContext rendererEmitter
  addContext finalizeEmitterContext finalizeEmitter

  game --初期化

  H.lift $ do
    ctx <- liftEffect $ getContext2D canvas

    -- 裏画面の生成
    subCanvas <- liftEffect $ createCanvasElement
    subCtx <- liftEffect $ getContext2D subCanvas
    width <- liftEffect $ getCanvasWidth canvas
    height <- liftEffect $ getCanvasHeight canvas
    liftEffect $ setCanvasHeight subCanvas height
    liftEffect $ setCanvasWidth subCanvas width

    hashMap <- liftAff $ loadSprites sprites

    let
      images = \x -> lookup x hashMap

    deltaTimerRef <- liftEffect $ new =<< nowTime

    -- renderループ
    forever $ do
      procStart <- liftEffect nowTime

      deltaTimer <- liftEffect $ read deltaTimerRef
      let
        Milliseconds deltaTimeMilliseconds = diff procStart deltaTimer
      liftEffect $ write procStart deltaTimerRef

      pics <- emit rendererEmitter { deltaTime: deltaTimeMilliseconds / 1000.0 }
      liftEffect $ clearRect subCtx { x: 0.0, y: 0.0, height, width }
      liftEffect $ drawPicture subCtx images $ fold pics
      let
        imgSource = canvasElementToImageSource subCanvas
      liftEffect $ clearRect ctx { x: 0.0, y: 0.0, height, width }
      liftEffect $ drawImage ctx imgSource 0.0 0.0

      procEnd <- liftEffect nowTime
      let
        Milliseconds dt = diff procEnd procStart

      -- fps調整
      liftAff $ delay $ Milliseconds $ max 0.0 $ 1000.0 / fps - dt
