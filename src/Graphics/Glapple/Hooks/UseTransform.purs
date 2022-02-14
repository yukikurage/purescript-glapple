module Graphics.Glapple.Hooks.UseTransform where

import Prelude

import Control.Monad.Reader (ask)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref (modify_, read)
import Graphics.Canvas (Transform)
import Graphics.Glapple.Data.Component (Component, ComponentTransform(..), componentTransformToTransform)
import Graphics.Glapple.Util ((|*|))

useTransform
  :: forall sprite
   . Component sprite (Effect Transform)
useTransform = do
  { componentTransform } <- ask
  pure $ (componentTransformToTransform <$> read componentTransform)

useTranslate
  :: forall sprite
   . Component sprite
       ( Effect { x :: Number, y :: Number } /\
           ( { x :: Number
             , y :: Number
             }
             -> Effect Unit
           )
       )
useTranslate = do
  { componentTransform } <- ask
  pure $
    ( (\(ComponentTransform comTrans) -> comTrans.translate) <$> read
        componentTransform
    ) /\
      ( \tl -> modify_
          ( \(ComponentTransform comTrans) -> ComponentTransform $ comTrans
              { translate = tl }
          )
          componentTransform
      )

useTranslateNow
  :: forall sprite. { x :: Number, y :: Number } -> Component sprite Unit
useTranslateNow trans = do
  _ /\ setTranslate <- useTranslate
  liftEffect $ setTranslate trans

useScale
  :: forall sprite
   . Component sprite
       ( Effect { x :: Number, y :: Number } /\
           ({ x :: Number, y :: Number } -> Effect Unit)
       )
useScale = do
  { componentTransform } <- ask
  pure $
    ( (\(ComponentTransform comTrans) -> comTrans.scale) <$> read
        componentTransform
    ) /\
      ( \sc -> modify_
          ( \(ComponentTransform comTrans) -> ComponentTransform $ comTrans
              { scale = sc }
          )
          componentTransform
      )

useScaleNow
  :: forall sprite. { x :: Number, y :: Number } -> Component sprite Unit
useScaleNow sc = do
  _ /\ setScale <- useScale
  liftEffect $ setScale sc

useRotate
  :: forall sprite
   . Component sprite (Effect Number /\ (Number -> Effect Unit))
useRotate = do
  { componentTransform } <- ask
  pure $
    ( (\(ComponentTransform comTrans) -> comTrans.rotate) <$> read
        componentTransform
    ) /\
      ( \rot -> modify_
          ( \(ComponentTransform comTrans) -> ComponentTransform $ comTrans
              { rotate = rot }
          )
          componentTransform
      )

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
    pure $ comTrans0 |*|
      componentTransformToTransform comTrans1

useGlobalTranslate
  :: forall sprite. Component sprite (Effect { x :: Number, y :: Number })
useGlobalTranslate = do
  getGlobalTransform <- useGlobalTransform
  pure do
    { m31, m32 } <- getGlobalTransform
    pure { x: m31, y: m32 }
