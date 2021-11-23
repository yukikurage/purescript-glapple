module Graphics.Glapple.GlappleM where

import Prelude

import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Reader (class MonadAsk, ReaderT, ask, runReaderT)
import Control.Monad.State (lift)
import Data.Maybe (Maybe(..))
import Data.Set (Set, member)
import Data.Time (Time, diff)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Effect.Now (nowTime)
import Effect.Ref (Ref, modify_, read, write)
import Graphics.Glapple.Data.Emitter (EmitterId, fire)
import Graphics.Glapple.Data.Event (Event, KeyCode)
import Graphics.Glapple.Data.InternalRegistrationIds (InternalRegistrationIds, unregisterGame)

type InternalState (s :: Type) g (i :: Type) o =
  { eventEmitter :: EmitterId Effect Event
  , outputEmitter :: EmitterId Effect o
  , initTimeRef :: Ref (Maybe Time) --ルートゲーム開始時の時刻
  , localInitTimeRef :: Ref (Maybe Time) --ゲームがrunされた時刻
  , gameStateRef :: Ref (Maybe g) --ゲームの状態を保存(ゲーム開始前はNothing)
  , internalRegistrationIdsRef :: Ref (Maybe (InternalRegistrationIds s i o)) --ゲームのregistrationIdを保存
  , keyStateRef :: Ref (Set KeyCode) --現在押されているキーのSet
  , mousePositionRef :: Ref (Maybe { mouseX :: Number, mouseY :: Number })
  }

newtype GlappleM s g i o a =
  GlappleM (ReaderT (InternalState s g i o) (MaybeT Effect) a)

derive newtype instance Functor (GlappleM s g i o)
derive newtype instance Apply (GlappleM s g i o)
derive newtype instance Applicative (GlappleM s g i o)
derive newtype instance Bind (GlappleM s g i o)
derive newtype instance Monad (GlappleM s g i o)
derive newtype instance
  MonadAsk (InternalState s g i o)
    (GlappleM s g i o)

runGlappleM
  :: forall s g i o a
   . GlappleM s g i o a
  -> InternalState s g i o
  -> Effect (Maybe a)
runGlappleM (GlappleM state) f = runMaybeT $ runReaderT state f

instance MonadEffect (GlappleM s g i o) where
  liftEffect e = GlappleM $ lift $ lift e

-- | Get current game state.
getGameState
  :: forall s g i o
   . GlappleM s g i o g
getGameState = do
  { gameStateRef } <- ask
  gameState <- liftEffect $ read gameStateRef
  GlappleM $ lift $ MaybeT $ pure gameState

-- | Put game state
putGameState :: forall s g i o. g -> GlappleM s g i o Unit
putGameState x = do
  { gameStateRef } <- ask
  liftEffect $ write (Just x) gameStateRef

-- | Modify function to game state
modifyGameState
  :: forall s g i o
   . (g -> g)
  -> GlappleM s g i o Unit
modifyGameState f = do
  { gameStateRef } <- ask
  liftEffect $ modify_ (map f) gameStateRef

-- | Get the time since the root game was started.
getGlobalTime
  :: forall s g i o
   . GlappleM s g i o Number
getGlobalTime = do
  { initTimeRef } <- ask
  initTimeMaybe <- liftEffect $ read initTimeRef
  nowT <- liftEffect $ nowTime
  let
    f initTime = t / 1000.0
      where
      Milliseconds t = diff nowT initTime
    totalTime = map f initTimeMaybe
  GlappleM $ lift $ MaybeT $ pure totalTime

-- | Get the time since the current game was started.
getLocalTime
  :: forall s g i o
   . GlappleM s g i o Number
getLocalTime = do
  { localInitTimeRef } <- ask
  initTimeMaybe <- liftEffect $ read localInitTimeRef
  nowT <- liftEffect $ nowTime
  let
    f initTime = t / 1000.0
      where
      Milliseconds t = diff nowT initTime
    totalTime = map f initTimeMaybe
  GlappleM $ lift $ MaybeT $ pure totalTime

-- | Raise output to the parent game.
raise
  :: forall s g i o
   . o
  -> GlappleM s g i o Unit
raise output = do
  { outputEmitter } <- ask --outputのエミッターを取得
  liftEffect $ fire outputEmitter output --発火

-- | Destroy the game.
destroy :: forall s g i o. GlappleM s g i o Unit
destroy = do
  { internalRegistrationIdsRef } <- ask
  internalRegistrationIdsMaybe <- liftEffect $ read internalRegistrationIdsRef
  case internalRegistrationIdsMaybe of
    Just x -> do
      unregisterGame x
    Nothing -> log "Glapple Warning: 初期化前に destroy が呼ばれたので，ゲームを破棄できませんでした．意図していない挙動であるかもしれません．"
  GlappleM $ lift $ MaybeT $ pure Nothing

-- | Gets the current key press state.
getKeyState :: forall s g i o. KeyCode -> GlappleM s g i o Boolean
getKeyState code = do
  { keyStateRef } <- ask
  keyState <- liftEffect $ read keyStateRef
  pure $ member code keyState

-- | Gets the current mouse position.
-- | Returns Nothing if the mouse position cannot be obtained because the user has not moved the mouse yet.
getMousePosition :: forall s g i o. GlappleM s g i o (Maybe { mouseX :: Number, mouseY :: Number })
getMousePosition = do
  { mousePositionRef } <- ask
  mousePosition <- liftEffect $ read mousePositionRef
  pure mousePosition

-- | Stop the current process.
break :: forall s g i o a. GlappleM s g i o a
break = GlappleM $ lift $ MaybeT $ pure Nothing

-- | Express the process of GlappleM by Effect.
toEffect :: forall s g i o a x. (x -> GlappleM s g i o a) -> GlappleM s g i o (x -> Effect (Maybe a))
toEffect glappleM = do
  internalState <- ask
  pure $ \x -> runGlappleM (glappleM x) internalState