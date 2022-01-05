module Graphics.Glapple.Runner
  ( addRenderer
  , useRenderer
  , useUpdate
  ) where

import Prelude

import Control.Applicative.Indexed (ipure)
import Control.Monad.Indexed.Qualified as Ix
import Control.Monad.Rec.Class (class MonadRec, forever)
import Data.Array (fold)
import Data.HashMap (lookup)
import Data.Hashable (class Hashable)
import Data.Time (diff)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Now (nowTime)
import Graphics.Canvas (CanvasElement, canvasElementToImageSource, clearRect, drawImage, getCanvasHeight, getCanvasWidth, getContext2D, setCanvasHeight, setCanvasWidth)
import Graphics.Glapple.Data.Emitter (Emitter, addListener, emit, newEmitter, removeListener)
import Graphics.Glapple.Data.Picture (Picture, drawPicture)
import Graphics.Glapple.Data.Sprite (Sprite, loadSprites)
import Graphics.Glapple.GlappleM (Context(..), GlappleM, addContext, ilift, useContext)
import Prim.Row (class Lacks)

foreign import createCanvasElement :: Effect CanvasElement

rendererEmitterContext
  :: forall sprite m
   . Context "rendererEmitter" (Emitter Unit (Picture sprite) m)
rendererEmitterContext = Context

-- | レンダラーを追加
-- | 一つのプロジェクトにつき高々一回
addRenderer
  :: forall r m sprite
   . MonadAff m
  => MonadRec m
  => Lacks "rendererEmitter" r
  => Hashable sprite
  => Number
  -> Array (Sprite sprite)
  -> CanvasElement
  -> GlappleM m
       r
       (rendererEmitter :: Emitter Unit (Picture sprite) m | r)
       Unit

addRenderer fps sprites canvas = Ix.do
  rendererEmitter <- ilift $ newEmitter
  addContext rendererEmitterContext rendererEmitter

  ilift $ do
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

    -- renderループ
    forever $ do
      procStart <- liftEffect nowTime

      pics <- emit rendererEmitter unit
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

-- | レンダラーを使用
-- | 返り値はレンダラーの解除アクション
useRenderer
  :: forall m r sprite
   . MonadEffect m
  => (Unit -> m (Picture sprite))
  -> GlappleM m
       (rendererEmitter :: Emitter Unit (Picture sprite) m | r)
       (rendererEmitter :: Emitter Unit (Picture sprite) m | r)
       (m Unit)
useRenderer renderer = Ix.do
  rendererEmitter <- useContext rendererEmitterContext
  registration <- ilift $ addListener rendererEmitter renderer
  ipure $ removeListener registration

useUpdate
  :: forall m r sprite
   . MonadEffect m
  => (Unit -> m Unit)
  -> GlappleM m
       (rendererEmitter :: Emitter Unit (Picture sprite) m | r)
       (rendererEmitter :: Emitter Unit (Picture sprite) m | r)
       (m Unit)
useUpdate updateHandler = Ix.do
  rendererEmitter <- useContext rendererEmitterContext
  registration <- ilift $ addListener rendererEmitter \_ -> do
    updateHandler unit
    pure mempty
  ipure $ removeListener registration
