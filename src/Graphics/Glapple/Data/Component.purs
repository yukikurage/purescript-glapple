module Graphics.Glapple.Data.Component
  ( Component(..)
  , Internal
  , runComponent
  ) where

import Prelude

import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT, runReaderT)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Maybe (Maybe)
import Data.Set (Set)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Ref (Ref)
import Graphics.Canvas (CanvasImageSource, Context2D)
import Graphics.Glapple.Data.Complex (Complex)
import Graphics.Glapple.Data.Emitter (Emitter)
import Graphics.Glapple.Data.KeyEvent (KeyCode, KeyEvent)
import Graphics.Glapple.Data.Transform (Transform)

type Internal sprite =
  { rendererEmitter ::
      Emitter
        { ctx :: Context2D
        , canvasImageSources :: sprite -> Maybe CanvasImageSource
        , deltaTime :: Number
        }
        Effect
  , finalizeEmitter :: Emitter Unit Effect
  , rayEmitter :: Emitter Complex Effect
  , keyEmitter :: Emitter KeyEvent Effect
  , keyStateRef :: Ref (Set KeyCode)
  , mouseStateRef :: Ref Complex
  , parentTransform :: Effect Transform
  , componentTransform :: Ref Transform
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
