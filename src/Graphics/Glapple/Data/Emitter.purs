module Graphics.Glapple.Data.Emitter where

import Prelude

import Data.HashMap (HashMap, delete, empty, insert, values)
import Data.HashSet (HashSet, fromFoldable)
import Data.Hashable (class Hashable)
import Data.Traversable (for)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Random (random)
import Effect.Ref (Ref, modify_, new, read)

type Key = Number

type Internal :: forall k. Type -> k -> (k -> Type) -> Type
type Internal event response m = (Ref (HashMap Key (event -> m response)))

data Registration :: forall k. Type -> k -> (k -> Type) -> Type
data Registration event response m = Registration Key
  (Internal event response m)

newtype Emitter :: forall k. Type -> k -> (k -> Type) -> Type
newtype Emitter event response m = Emitter (Internal event response m)

newEmitter
  :: forall m event response. MonadEffect m => m (Emitter event response m)
newEmitter = Emitter <$> (liftEffect $ new empty)

addListener
  :: forall m event response
   . MonadEffect m
  => Emitter event response m
  -> (event -> m response)
  -> m (Registration event response m)
addListener (Emitter ref) listener = do
  key <- liftEffect $ random
  liftEffect $ modify_ (insert key listener) ref
  pure $ Registration key ref

removeListener
  :: forall m event response
   . MonadEffect m
  => Registration event response m
  -> m Unit
removeListener (Registration key ref) = liftEffect $ modify_
  (delete key)
  ref

emit
  :: forall m event response
   . MonadEffect m
  => Hashable response
  => Emitter event response m
  -> event
  -> m (HashSet response)
emit (Emitter ref) event = do
  listeners <- liftEffect $ read ref
  responses <- for (values listeners) $ \listener -> do
    response <- listener event
    pure response
  pure $ fromFoldable responses
