module Graphics.Glapple.Data.Hooks
  ( Hooks(..)
  , Internal
  , runHooks
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
  , hoverEmitter ::
      Emitter Complex Effect -- Emitter の prevent を利用してオブジェクトがホバーされているか否かを判定する
  , keyEmitter :: Emitter KeyEvent Effect
  , keyStateRef :: Ref (Set KeyCode)
  , mouseStateRef :: Ref Complex
  , parentTransform :: Effect Transform
  , componentTransform :: Ref Transform
  }

newtype Hooks sprite a = Hooks (ReaderT (Internal sprite) Effect a)

derive newtype instance Functor (Hooks sprite)
derive newtype instance Apply (Hooks sprite)
derive newtype instance Applicative (Hooks sprite)
derive newtype instance Bind (Hooks sprite)
instance Monad (Hooks sprite)
derive newtype instance MonadAsk (Internal sprite) (Hooks sprite)

derive newtype instance MonadReader (Internal sprite) (Hooks sprite)

derive newtype instance MonadEffect (Hooks sprite)
derive newtype instance MonadRec (Hooks sprite)

runHooks
  :: forall sprite a
   . Internal sprite
  -> Hooks sprite a
  -> Effect a
runHooks emitter (Hooks m) = runReaderT m emitter
