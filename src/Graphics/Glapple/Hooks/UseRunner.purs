module Graphics.Glapple.Hooks.UseRunner
  ( useChildRunner
  , useChildRunnerNow
  , useRunner
  , useRunnerNow
  ) where

import Prelude

import Control.Monad.Reader (ask)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref (new)
import Graphics.Glapple.Data.Component (Component, runComponent)
import Graphics.Glapple.Data.Emitter (addListener_, emit, newEmitter, size)
import Graphics.Glapple.Data.Transform (unitTransform)
import Graphics.Glapple.Hooks.UseTransform (useGlobalTransform)

-- | コンポーネントを生成
useRunner
  :: forall props sprite a
   . (props -> Component sprite a)
  -> Component sprite
       { run :: props -> Effect a, destroy :: Effect Unit, size :: Effect Int }
useRunner component = do
  allFinalizeEmitter <- liftEffect newEmitter
  { rendererEmitter, hoverEmitter, keyEmitter, keyStateRef, mouseStateRef } <-
    ask
  let
    run props = do
      finalizeEmitter <- newEmitter
      componentTransform <- new unitTransform
      _ <- addListener_ allFinalizeEmitter 0.0 \_ -> emit finalizeEmitter unit
        *>
          pure unit
      runComponent
        { rendererEmitter
        , hoverEmitter
        , finalizeEmitter
        , keyEmitter
        , keyStateRef
        , componentTransform
        , mouseStateRef
        , parentTransform: pure unitTransform
        } $
        component props
    destroy = emit allFinalizeEmitter unit *> pure unit
    size' = size allFinalizeEmitter
  pure { run, destroy, size: size' }

useRunnerNow
  :: forall props sprite a
   . (props -> Component sprite a)
  -> props
  -> Component sprite a
useRunnerNow component props = do
  { run } <- useRunner component
  liftEffect $ run props

-- | 子コンポーネントを生成
-- | 子コンポーネントのTransformは親のTransformが基準になる
useChildRunner
  :: forall props sprite a
   . (props -> Component sprite a)
  -> Component sprite
       { run :: props -> Effect a, destroy :: Effect Unit, size :: Effect Int }
useChildRunner component = do
  allFinalizeEmitter <- liftEffect newEmitter
  getGlobalTransform <- useGlobalTransform
  { rendererEmitter
  , hoverEmitter
  , keyEmitter
  , keyStateRef
  , mouseStateRef
  } <- ask
  let
    run props = do
      finalizeEmitter <- newEmitter
      componentTransform <- new unitTransform
      _ <- addListener_ allFinalizeEmitter 0.0 \_ -> emit finalizeEmitter unit
        *>
          pure unit
      runComponent
        { rendererEmitter
        , hoverEmitter
        , finalizeEmitter
        , keyEmitter
        , keyStateRef
        , componentTransform
        , mouseStateRef
        , parentTransform: getGlobalTransform
        } $
        component props
    destroy = emit allFinalizeEmitter unit *> pure unit
    size' = size allFinalizeEmitter
  pure { run, destroy, size: size' }

useChildRunnerNow
  :: forall props sprite a
   . (props -> Component sprite a)
  -> props
  -> Component sprite a
useChildRunnerNow component props = do
  { run } <- useChildRunner component
  liftEffect $ run props
