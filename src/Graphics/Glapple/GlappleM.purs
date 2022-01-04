module Graphics.Glapple.GlappleM where

import Prelude

import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT)

newtype GlappleM :: forall k. Type -> (k -> Type) -> k -> Type
newtype GlappleM game m x = GlappleM (ReaderT game m x)

derive newtype instance Functor m => Functor (GlappleM game m)
derive newtype instance Apply m => Apply (GlappleM game m)
derive newtype instance Applicative m => Applicative (GlappleM game m)
derive newtype instance Bind m => Bind (GlappleM game m)
derive newtype instance Monad m => Monad (GlappleM game m)
derive newtype instance MonadAsk game m => MonadAsk game (GlappleM game m)
derive newtype instance MonadReader game m => MonadReader game (GlappleM game m)
