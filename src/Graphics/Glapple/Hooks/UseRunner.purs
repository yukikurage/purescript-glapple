module Graphics.Glapple.Hooks.UseRunner
  ( useChildRunner
  , useChildRunnerNow
  , useRunner
  , useRunnerNow
  ) where

import Prelude

import Control.Monad.Reader (ask)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref (new)
import Graphics.Glapple.Data.Component (Component, runComponent)
import Graphics.Glapple.Data.Emitter (addListener_, emit, newEmitter)
import Graphics.Glapple.Data.Transform (unitTransform)
import Graphics.Glapple.Hooks.UseTransform (useGlobalTransform)

-- | コンポーネントを生成
useRunner
  :: forall props sprite a
   . (props -> Component sprite a)
  -> Component sprite ((props -> Effect a) /\ Effect Unit)
useRunner component = do
  allFinalizeEmitter <- liftEffect newEmitter
  { rendererEmitter, rayEmitter, keyEmitter, keyStateRef, mouseStateRef } <- ask
  let
    runner props = do
      finalizeEmitter <- newEmitter
      componentTransform <- new unitTransform
      _ <- addListener_ allFinalizeEmitter 0.0 \_ -> emit finalizeEmitter unit
        *>
          pure unit
      runComponent
        { rendererEmitter
        , rayEmitter
        , finalizeEmitter
        , keyEmitter
        , keyStateRef
        , componentTransform
        , mouseStateRef
        , parentTransform: pure unitTransform
        } $
        component props
    destroyer = emit allFinalizeEmitter unit *> pure unit
  pure $ runner /\ destroyer

useRunnerNow
  :: forall props sprite a
   . (props -> Component sprite a)
  -> props
  -> Component sprite a
useRunnerNow component props = do
  runner /\ _ <- useRunner component
  liftEffect $ runner props

-- | 子コンポーネントを生成
-- | 子コンポーネントのTransformは親のTransformが基準になる
useChildRunner
  :: forall props sprite a
   . (props -> Component sprite a)
  -> Component sprite ((props -> Effect a) /\ Effect Unit)
useChildRunner component = do
  allFinalizeEmitter <- liftEffect newEmitter
  getGlobalTransform <- useGlobalTransform
  { rendererEmitter
  , rayEmitter
  , keyEmitter
  , keyStateRef
  , mouseStateRef
  } <- ask
  let
    runner props = do
      finalizeEmitter <- newEmitter
      componentTransform <- new unitTransform
      _ <- addListener_ allFinalizeEmitter 0.0 \_ -> emit finalizeEmitter unit
        *>
          pure unit
      runComponent
        { rendererEmitter
        , rayEmitter
        , finalizeEmitter
        , keyEmitter
        , keyStateRef
        , componentTransform
        , mouseStateRef
        , parentTransform: getGlobalTransform
        } $
        component props
    destroyer = emit allFinalizeEmitter unit *> pure unit
  pure $ runner /\ destroyer

useChildRunnerNow
  :: forall props sprite a
   . (props -> Component sprite a)
  -> props
  -> Component sprite a
useChildRunnerNow component props = do
  runner /\ _ <- useChildRunner component
  liftEffect $ runner props
