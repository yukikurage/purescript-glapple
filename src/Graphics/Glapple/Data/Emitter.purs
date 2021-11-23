module Graphics.Glapple.Data.Emitter (newEmitter, unregister, unregisterAll, register, fire, EmitterId, RegistrationId, emitterSize) where

import Prelude

import Control.Monad.Rec.Class (class MonadRec)
import Control.Safely (for_)
import Data.Map (Map, delete, empty, insert, size)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref, new, read, write)

type KeyId = Int --serial number
{-
シリアルナンバーの衝突
Serial number conflict

同じシリアルナンバーが現れるのは60FPSでマイフレーム100個のハンドラを登録したとしても200時間程度かかります．
It takes about 200 hours for the same serial number to appear, even if you register 100 handlers for my frames at 60FPS.
-}

type Emitter m i = Int /\ Map KeyId (i -> m Unit) -- Max /\ Map

newtype EmitterId m i = EmitterId (Ref (Emitter m i))

-- |  The key that comes back when you register to the Emitter.
data RegistrationId m i = RegistrationId (EmitterId m i) KeyId

-- | Create a new emitter
newEmitter :: forall m m' i. MonadEffect m => m (EmitterId m' i)
newEmitter = do
  emitterId <- liftEffect $ new $ bottom /\ empty
  pure $ EmitterId emitterId

-- | Delete all handlers.
unregisterAll :: forall m m' i. MonadEffect m => EmitterId m' i -> m Unit
unregisterAll (EmitterId emitterId) = liftEffect $ write (bottom /\ empty) emitterId

-- | Fire emitter.
fire :: forall m i. MonadEffect m => MonadRec m => EmitterId m i -> i -> m Unit
fire (EmitterId emitterRef) i = do
  _ /\ emitter <- liftEffect $ read emitterRef
  for_ emitter (\f -> f i)

-- | Register a handler to the Emitter.
register :: forall m m' i. MonadEffect m => EmitterId m' i -> (i -> m' Unit) -> m (RegistrationId m' i)
register (EmitterId emitterRef) handler = do
  maxKey /\ emitter <- liftEffect $ read emitterRef
  let
    newKey = maxKey
    newMaxKey = if maxKey == top then bottom else maxKey + 1
    newE = newMaxKey /\ insert newKey handler emitter
  liftEffect $ write newE emitterRef
  pure (RegistrationId (EmitterId emitterRef) newKey)

-- | Remove the handler from the emitter.
unregister :: forall m m' i. MonadEffect m => RegistrationId m' i -> m Unit
unregister (RegistrationId (EmitterId emitterRef) key) = do
  maxKey /\ emitter <- liftEffect $ read emitterRef
  let
    newE = maxKey /\ delete key emitter
  liftEffect $ write newE emitterRef

emitterSize :: forall m' i m. MonadEffect m => EmitterId m' i -> m Int
emitterSize (EmitterId emitterRef) = do
  _ /\ emitter <- liftEffect $ read emitterRef
  pure $ size emitter