module Graphics.Glapple.Data.Component
  ( Component(..)
  , ComponentTransform(..)
  , Internal
  , componentTransformToTransform
  , runComponent
  , unitComponentTransform
  ) where

import Prelude

import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT, runReaderT)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Maybe (Maybe)
import Data.Set (Set)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Ref (Ref)
import Graphics.Canvas (CanvasImageSource, Context2D, Transform)
import Graphics.Glapple.Data.Emitter (Emitter)
import Graphics.Glapple.Data.KeyEvent (KeyCode, KeyEvent)
import Graphics.Glapple.Util (rotate, scale, translate, (|*|))

data ComponentTransform = ComponentTransform
  { translate :: { x :: Number, y :: Number }
  , scale :: { x :: Number, y :: Number }
  , rotate :: Number
  }

unitComponentTransform :: ComponentTransform
unitComponentTransform = ComponentTransform
  { translate: { x: 0.0, y: 0.0 }, scale: { x: 1.0, y: 1.0 }, rotate: 0.0 }

componentTransformToTransform :: ComponentTransform -> Transform
componentTransformToTransform
  ( ComponentTransform
      { translate: { x: trX, y: trY }, scale: { x: scX, y: scY }, rotate: rt }
  ) = translate trX trY
  |*| scale scX scY
  |*| rotate rt

type Internal sprite =
  { rendererEmitter ::
      Emitter
        { ctx :: Context2D
        , canvasImageSources :: sprite -> Maybe CanvasImageSource
        , deltaTime :: Number
        }
        Effect
  , finalizeEmitter :: Emitter Unit Effect
  , rayEmitter :: Emitter { x :: Number, y :: Number } Effect
  , keyEmitter :: Emitter KeyEvent Effect
  , keyStateRef :: Ref (Set KeyCode)
  , mouseStateRef :: Ref { x :: Number, y :: Number }
  , parentTransform :: Effect Transform
  , componentTransform :: Ref ComponentTransform
  }

newtype Component sprite a = Component (ReaderT (Internal sprite) Effect a)

derive newtype instance Functor (Component sprite)
derive newtype instance Apply (Component sprite)
derive newtype instance Applicative (Component sprite)
derive newtype instance Bind (Component sprite)
instance Monad (Component sprite)
derive newtype instance MonadAsk (Internal sprite) (Component sprite)

derive newtype instance MonadReader (Internal sprite) (Component sprite)

derive newtype instance MonadEffect (Component sprite)
derive newtype instance MonadRec (Component sprite)

runComponent
  :: forall sprite a
   . Internal sprite
  -> Component sprite a
  -> Effect a
runComponent emitter (Component m) = runReaderT m emitter
