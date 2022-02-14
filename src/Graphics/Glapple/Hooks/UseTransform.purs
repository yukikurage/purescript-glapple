module Graphics.Glapple.Hooks.UseTransform where

import Prelude

import Control.Monad.Reader (ask)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref (modify_, read)
import Graphics.Glapple.Data.Complex (Complex)
import Graphics.Glapple.Data.Component (Component)
import Graphics.Glapple.Data.Transform (Transform, computeChildTransform, modifyRotate, modifyTranslate, rotate, translate)

useTransform
  :: forall sprite
   . Component sprite (Effect Transform)
useTransform = do
  { componentTransform } <- ask
  pure $ read componentTransform

useTranslate
  :: forall sprite
   . Component sprite (Effect Complex /\ (Complex -> Effect Unit))
useTranslate = do
  { componentTransform } <- ask
  pure $ (translate <$> read componentTransform) /\
    (\cm -> modify_ (modifyTranslate cm) componentTransform)

useTranslateNow
  :: forall sprite. Complex -> Component sprite Unit
useTranslateNow trans = do
  _ /\ setTranslate <- useTranslate
  liftEffect $ setTranslate trans

useRotate
  :: forall sprite
   . Component sprite (Effect Number /\ (Number -> Effect Unit))
useRotate = do
  { componentTransform } <- ask
  pure $ (rotate <$> read componentTransform) /\
    (\cm -> modify_ (modifyRotate cm) componentTransform)

useRotateNow :: forall sprite. Number -> Component sprite Unit
useRotateNow rot = do
  _ /\ setRotate <- useRotate
  liftEffect $ setRotate rot

useGlobalTransform
  :: forall sprite
   . Component sprite (Effect Transform)
useGlobalTransform = do
  { componentTransform, parentTransform } <- ask
  pure do
    comTrans0 <- parentTransform
    comTrans1 <- read componentTransform
    pure $ computeChildTransform comTrans0 comTrans1

useGlobalTranslate
  :: forall sprite. Component sprite (Effect Complex)
useGlobalTranslate = do
  getGlobalTransform <- useGlobalTransform
  pure do
    trans <- getGlobalTransform
    pure $ translate trans
