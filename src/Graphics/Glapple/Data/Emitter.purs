module Graphics.Glapple.Data.Emitter
  ( Emitter(..)
  , Key
  , Priority
  , addListener
  , addListener_
  , emit
  , newEmitter
  , removeAllListener
  , size
  ) where

import Prelude

import Control.Monad.Rec.Class (class MonadRec)
import Control.Safely (for_)
import Data.Map (Map, delete, empty, insert)
import Data.Map as M
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Random (random)
import Effect.Ref (Ref, modify_, new, read, write)

type Key = Number /\ Number -- Priority /\ Random 優先度が高いほど先に実行される

type Priority = Int

newtype Emitter event m = Emitter
  (Ref (Map Key (event -> m Unit -> m Unit)))

newEmitter
  :: forall m event. MonadEffect m => m (Emitter event m)
newEmitter = Emitter <$> (liftEffect $ new empty)

addListener_
  :: forall m event
   . MonadEffect m
  => Emitter event m
  -> Number
  -> (event -> m Unit)
  -> m (m Unit)
addListener_ (Emitter ref) priority listenerTemp = do
  key <- liftEffect $ random
  let
    listener event _ = listenerTemp event
  liftEffect $ modify_ (insert (-priority /\ key) $ listener) ref
  pure $ liftEffect $ modify_ (delete (-priority /\ key)) ref

addListener
  :: forall m event
   . MonadEffect m
  => Emitter event m
  -> Number
  -> (event -> (m Unit) -> m Unit)
  -> m (m Unit)
addListener (Emitter ref) priority listener = do
  key <- liftEffect $ random
  liftEffect $ modify_
    (insert (-priority /\ key) listener)
    ref
  pure $ liftEffect $ modify_ (delete (-priority /\ key)) ref

emit
  :: forall m event
   . MonadEffect m
  => MonadRec m
  => Emitter event m
  -> event
  -> m Unit
emit (Emitter ref) event = do
  emitter <- liftEffect $ read ref
  isContinueRef <- liftEffect $ new true

  let
    prevent = liftEffect $ write false isContinueRef
  for_ emitter \listener -> do
    isContinue <- liftEffect $ read isContinueRef
    if isContinue then do
      listener event prevent
    else pure unit

size :: forall event m. MonadEffect m => Emitter event m -> m Int
size (Emitter ref) = liftEffect do
  s <- read ref
  pure $ M.size s

removeAllListener
  :: forall event m
   . MonadEffect m
  => Emitter event m
  -> m Unit
removeAllListener (Emitter ref) = liftEffect $ write empty ref
