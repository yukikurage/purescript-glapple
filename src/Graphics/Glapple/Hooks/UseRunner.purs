module Graphics.Glapple.Hooks.UseRunner where

import Prelude

import Control.Monad.Reader (ask)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref (new)
import Graphics.Canvas (Transform)
import Graphics.Glapple.Data.Component (Component, runComponent)
import Graphics.Glapple.Data.Emitter (addListener_, emit, newEmitter)
import Graphics.Glapple.Hooks.UseTransform (useGlobalTransform)
import Graphics.Glapple.Util (unitTransform)

-- | コンポーネントを生成
useRunner
  :: forall sprite a
   . Component sprite a
  -> Component sprite ((Transform -> Effect a) /\ Effect Unit)
useRunner component = do
  allFinalizeEmitter <- liftEffect newEmitter
  { rendererEmitter, rayEmitter, keyEmitter, keyStateRef, mouseStateRef } <- ask
  let
    runner trans = do
      finalizeEmitter <- newEmitter
      componentTransform <- new trans
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
        }
        component
    destroyer = emit allFinalizeEmitter unit *> pure unit
  pure $ runner /\ destroyer

-- | 子コンポーネントを生成
-- | 子コンポーネントのTransformは親のTransformが基準になる
useChildRunner
  :: forall sprite a
   . Component sprite a
  -> Component sprite ((Transform -> Effect a) /\ Effect Unit)
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
    runner trans = do
      finalizeEmitter <- newEmitter
      componentTransform <- new trans
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
        }
        component
    destroyer = emit allFinalizeEmitter unit *> pure unit
  pure $ runner /\ destroyer
