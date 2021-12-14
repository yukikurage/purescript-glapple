module Graphics.Glapple.Data.Picture
  ( (<-*)
  , (<-+)
  , (<-.)
  , (<-^)
  , (|*|)
  , DrawStyle(..)
  , Font(..)
  , FontFamily(..)
  , FontStyle(..)
  , FontWeight(..)
  , Picture
  , Shape(..)
  , absolute
  , absorb
  , absorb'
  , addComposite
  , angleToTransform
  , arc
  , color
  , destinationOverComposite
  , drawPicture
  , drawWithTransform
  , empty
  , fan
  , font
  , line
  , lineWidth
  , multiplyComposite
  , composite
  , multiplyTransform
  , opacity
  , paint
  , polygon
  , rect
  , rotate
  , scale
  , sourceOverComposite
  , sprite
  , text
  , textAlign
  , textBaseLine
  , transform
  , translate
  , translateToTransform
  , tryLoadImageEffect
  ) where

import Prelude

import Color (Color, cssStringRGBA)
import Control.Safely (for_)
import Data.Array (uncons)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (Aff, error, makeAff)
import Effect.Class (class MonadEffect, liftEffect)
import Graphics.Canvas (CanvasGradient, CanvasImageSource, CanvasPattern, Composite(..), Context2D, PatternRepeat, TextAlign, TextBaseline, Transform, addColorStop, beginPath, closePath, createLinearGradient, createPattern, createRadialGradient, drawImage, fill, lineTo, moveTo, restore, save, setGlobalAlpha, setGlobalCompositeOperation, setGradientFillStyle, setLineWidth, setPatternFillStyle, setTextAlign, setTextBaseline, setTransform, stroke, tryLoadImage)
import Graphics.Canvas as C
import Math (cos, floor, pi, sin)

newtype Picture sprite = Picture
  (Context2D -> (sprite -> Maybe CanvasImageSource) -> Effect Unit)

instance Semigroup (Picture sprite) where
  append = composite SourceOver

instance Monoid (Picture sprite) where
  mempty = empty

-- | Draw a picture
-- | It is not recommended to use this except for the glapple system.
drawPicture
  :: forall sprite
   . Context2D
  -> (sprite -> Maybe CanvasImageSource)
  -> Picture sprite
  -> Effect Unit
drawPicture ctx canvasImageSources (Picture f) = do
  f ctx canvasImageSources

saveAndRestore :: forall m. MonadEffect m => Context2D -> m Unit -> m Unit
saveAndRestore ctx f = do
  liftEffect $ save ctx
  f
  liftEffect $ restore ctx

-- | Effect version of tryLoadImage
tryLoadImageEffect :: String -> Aff CanvasImageSource
tryLoadImageEffect str = makeAff \thrower -> do
  tryLoadImage str $ case _ of
    Just x -> thrower $ Right x
    Nothing -> thrower $ Left $ error $ "Image LoadingError: " <> str
  pure mempty

data DrawStyle sprite
  = LinearGradient
      { x0 :: Number
      , y0 :: Number
      , x1 :: Number
      , y1 :: Number
      , colorStops :: Array (Number /\ Color)
      }
  | RadialGradient
      { x0 :: Number
      , y0 :: Number
      , r0 :: Number
      , x1 :: Number
      , y1 :: Number
      , r1 :: Number
      , colorStops :: Array (Number /\ Color)
      }
  | Pattern { sprite :: sprite, repeat :: PatternRepeat }
  | MonoColor Color

derive instance Eq sprite => Eq (DrawStyle sprite)

data Shape = Fill | Stroke

derive instance Eq Shape
derive instance Ord Shape

runShape :: Context2D -> Shape -> Effect Unit
runShape ctx = case _ of
  Fill -> do
    fill ctx
    beginPath ctx
  Stroke -> do
    stroke ctx
    beginPath ctx

foreign import setGradientStrokeStyle
  :: Context2D -> CanvasGradient -> Effect Unit

foreign import setPatternStrokeStyle
  :: Context2D -> CanvasPattern -> Effect Unit

foreign import getTransform :: Context2D -> Effect Transform

setDrawStyle
  :: forall s
   . Context2D
  -> (s -> Maybe CanvasImageSource)
  -> DrawStyle s
  -> Effect Unit
setDrawStyle ctx canvasImageSources = case _ of
  LinearGradient { x0, y0, x1, y1, colorStops } -> do
    gradient <- createLinearGradient ctx { x0, y0, x1, y1 }
    for_ colorStops \(x /\ col) -> addColorStop gradient x $ cssStringRGBA col
    setGradientFillStyle ctx gradient
    setGradientStrokeStyle ctx gradient
  RadialGradient { x0, y0, x1, y1, r0, r1, colorStops } -> do
    gradient <- createRadialGradient ctx { x0, y0, x1, y1, r0, r1 }
    for_ colorStops \(x /\ col) -> addColorStop gradient x $ cssStringRGBA col
    setGradientFillStyle ctx gradient
    setGradientStrokeStyle ctx gradient
  Pattern { sprite: spr, repeat } -> case canvasImageSources spr of
    Nothing -> pure unit
    Just canvasImageSource -> do
      pattern <- createPattern ctx canvasImageSource repeat
      setPatternFillStyle ctx pattern
      setPatternStrokeStyle ctx pattern
  MonoColor c -> do
    C.setFillStyle ctx $ cssStringRGBA c
    C.setStrokeStyle ctx $ cssStringRGBA c

data FontStyle = FontStyleNormal | Oblique | Italic

derive instance Eq FontStyle
derive instance Ord FontStyle

instance Show FontStyle where
  show = case _ of
    FontStyleNormal -> "normal"
    Oblique -> "oblique"
    Italic -> "italic"

data FontWeight = FontWeightNormal | Bold | FontWeight Int

derive instance Eq FontWeight
derive instance Ord FontWeight

instance Show FontWeight where
  show = case _ of
    FontWeightNormal -> "normal"
    Bold -> "bold"
    FontWeight i -> show i

data FontFamily = Serif | SansSerif | Cursive | Fantasy | Monospace | Manual String

derive instance Eq FontFamily
derive instance Ord FontFamily

instance Show FontFamily where
  show = case _ of
    Serif -> "serif"
    SansSerif -> "bold"
    Cursive -> "cursive"
    Fantasy -> "fantasy"
    Monospace -> "monospace"
    Manual str -> "\'" <> str <> "\'"

newtype Font = Font
  { fontStyle :: FontStyle
  , fontWeight :: FontWeight
  , fontSize :: Int
  , fontHeight :: Int
  , fontFamily :: FontFamily
  }

derive instance Eq Font
derive instance Ord Font

setFont :: Context2D -> Font -> Effect Unit
setFont ctx (Font { fontStyle, fontWeight, fontSize, fontHeight, fontFamily }) =
  do
    C.setFont ctx $ show fontStyle
      <> " "
      <> show fontWeight
      <> " "
      <> show fontSize
      <> "px/"
      <> show fontHeight
      <> "px "
      <> show fontFamily

------------------------
-- Picture Operations --
------------------------

-- | Combine images using the specified combining method.
-- | Issue: Synthesis of the combined image behaves unexpectedly.
-- | ex) x <-+ (y <-^ z) = (x <-+ y) <-^ z
composite
  :: forall sprite
   . Composite
  -> Picture sprite
  -> Picture sprite
  -> Picture sprite
composite comp pic1 pic2 =
  Picture \ctx canvasImageSources -> do
    drawPicture ctx canvasImageSources pic1
    liftEffect $ setGlobalCompositeOperation ctx comp
    drawPicture ctx canvasImageSources pic2

sourceOverComposite :: forall s. Picture s -> Picture s -> Picture s
sourceOverComposite = composite SourceOver

destinationOverComposite :: forall s. Picture s -> Picture s -> Picture s
destinationOverComposite = composite DestinationOver

multiplyComposite :: forall s. Picture s -> Picture s -> Picture s
multiplyComposite = composite Multiply

addComposite :: forall s. Picture s -> Picture s -> Picture s
addComposite = composite Lighter

infixl 5 sourceOverComposite as <-^
infixl 5 destinationOverComposite as <-.
infixl 5 multiplyComposite as <-*
infixl 5 addComposite as <-+

-- | Translate a picture.
translate :: forall s. Number -> Number -> Picture s -> Picture s
translate x y = operate
  (\ctx -> C.translate ctx { translateX: x, translateY: y })

-- | Scale a picture.
scale :: forall s. Number -> Number -> Picture s -> Picture s
scale sx sy = operate (\ctx -> C.scale ctx { scaleX: sx, scaleY: sy })

-- | Rotate a picture. (Clockwise)
rotate :: forall s. Number -> Picture s -> Picture s
rotate r = operate (flip C.rotate r)

transform :: forall s. Transform -> Picture s -> Picture s
transform trans = operate (flip C.transform trans)

------------
-- Magics --
------------

-- | Retrieves the current transform state and draws a Picture.
drawWithTransform :: forall s. (Transform -> Picture s) -> Picture s
drawWithTransform f = Picture \ctx img -> do
  t <- liftEffect $ getTransform ctx
  drawPicture ctx img $ f t

-- | It is not recommended to use this system except for the glapple system.
absorb'
  :: forall s
   . (Context2D -> (s -> Maybe CanvasImageSource) -> Effect (Picture s))
  -> Picture s
absorb' affPic = Picture \ctx img -> do
  pic <- affPic ctx img
  drawPicture ctx img pic

-- | Embeds the action of Effect into the Picture.
absorb :: forall s. Effect (Picture s) -> Picture s
absorb affPic = Picture \ctx img -> do
  pic <- affPic
  drawPicture ctx img pic

-- | Disables the parent's transform, forcing it to draw from the origin. Heavy use is deprecated.
absolute :: forall s. Picture s -> Picture s
absolute = operate
  ( flip setTransform
      { m11: 1.0, m12: 0.0, m21: 0.0, m22: 1.0, m31: 0.0, m32: 0.0 }
  )

------------
-- Shapes --
------------

operate :: forall s. (Context2D -> Effect Unit) -> Picture s -> Picture s
operate f p = Picture \ctx img -> saveAndRestore ctx do
  liftEffect $ f ctx
  drawPicture ctx img p

-- | Empty picture.
empty :: forall sprite. Picture sprite
empty = Picture \_ _ -> pure unit

-- | Draws the sprite loaded by runGame.
sprite :: forall sprite. sprite -> Picture sprite
sprite spr = Picture \ctx canvasImageSources -> liftEffect do
  case canvasImageSources spr of
    Nothing -> pure unit
    Just x -> do
      drawImage ctx x 0.0 0.0

-- | Add color or pattern.
paint :: forall s. DrawStyle s -> Picture s -> Picture s
paint drawStyle shape = Picture \ctx img -> saveAndRestore ctx do
  liftEffect $ setDrawStyle ctx img drawStyle
  drawPicture ctx img shape

-- | Set opacity.
opacity :: forall s. Number -> Picture s -> Picture s
opacity o = operate (flip setGlobalAlpha o)

-- | Set color
color :: forall s. Color -> Picture s -> Picture s
color c s = paint (MonoColor c) s

-- | Set textAlign.
textAlign :: forall s. TextAlign -> Picture s -> Picture s
textAlign a = operate (flip setTextAlign a)

-- | Set font.
font :: forall s. Font -> Picture s -> Picture s
font f = operate (flip setFont f)

-- | Set textBaseLine.
textBaseLine :: forall s. TextBaseline -> Picture s -> Picture s
textBaseLine b = operate (flip setTextBaseline b)

-- | Set lineWidth.
lineWidth :: forall s. Number -> Picture s -> Picture s
lineWidth w = operate $ flip setLineWidth w

-- | Draw text.
text
  :: forall sprite
   . Shape
  -> String
  -> Picture sprite
text style str = Picture \ctx _ -> saveAndRestore ctx $ liftEffect $ do
  save ctx
  beginPath ctx
  case style of
    Fill -> do
      C.fillText ctx str 0.0 0.0
    Stroke -> do
      C.strokeText ctx str 0.0 0.0
  restore ctx

-- | Draw polygon.
polygon
  :: forall sprite
   . Shape
  -> Array (Number /\ Number)
  -> Picture sprite
polygon style path = Picture \ctx _ -> saveAndRestore ctx $ liftEffect do
  case uncons path of
    Just { head: (hx /\ hy), tail } -> do
      moveTo ctx hx hy
      for_ tail \(x /\ y) -> lineTo ctx x y
      closePath ctx
    Nothing -> pure unit
  runShape ctx style

-- | Draw line.
line
  :: forall sprite
   . Array (Number /\ Number)
  -> Picture sprite
line path = Picture \ctx _ -> saveAndRestore ctx $ liftEffect do
  case uncons path of
    Just { head: (hx /\ hy), tail } -> do
      moveTo ctx hx hy
      for_ tail \(x /\ y) -> lineTo ctx x y
    Nothing -> pure unit
  runShape ctx $ Stroke

-- | Draw rectangle.
rect
  :: forall s
   . Shape
  -> Number
  -> Number
  -> Picture s
rect style width height = Picture \ctx _ -> saveAndRestore ctx $ liftEffect do
  C.rect ctx { x: 0.0, y: 0.0, height, width }
  runShape ctx style

-- | Draw an arc with stroke.
-- | Property angle is the rotation angle from the start position.
-- | A positive number will cause a clockwise rotation, 2π will make a perfect circle, and 4π will return to a blank state.
arc
  :: forall s
   . { start :: Number, angle :: Number, radius :: Number }
  -> Picture s
arc { start, angle, radius } = Picture \ctx _ -> saveAndRestore ctx $ liftEffect
  do
    let
      { start: start', end } = convertArcFormat { start, angle }
    C.arc ctx { x: 0.0, y: 0.0, start: start', end, radius }
    runShape ctx $ Stroke

convertArcFormat
  :: { start :: Number, angle :: Number } -> { start :: Number, end :: Number }
convertArcFormat { start, angle } = { start: startRes, end: endRes }
  where
  start' = start - floor (start / (2.0 * pi)) * 2.0 * pi
  angle' = angle - floor (angle / (4.0 * pi)) * 4.0 * pi
  startRes /\ endRes = case angle' of
    x | 0.0 <= x && x <= 2.0 * pi -> start' /\ (start' + x)
    x {-2 * pi < r && r < 4 * pi-} -> (start' + x - 2.0 * pi) /\ start'

-- | Draw a fan.
-- | Property angle is the rotation angle from the start position.
-- | A positive number will cause a clockwise rotation, 2π will make a perfect circle, and 4π will return to a blank state.
fan
  :: forall s
   . Shape
  -> { start :: Number, angle :: Number, radius :: Number }
  -> Picture s
fan style { radius, start, angle } = Picture \ctx _ -> saveAndRestore ctx $
  liftEffect do
    let
      { start: start', end } = convertArcFormat { start, angle }
    moveTo ctx 0.0 0.0
    C.arc ctx { x: 0.0, y: 0.0, start: start', end, radius }
    closePath ctx
    runShape ctx style

---------------------------
-- Transform Computation --
---------------------------

-- | convert angle to Transform
angleToTransform :: Number -> Transform
angleToTransform x =
  { m11: cos x, m12: sin x, m21: -sin x, m22: cos x, m31: 0.0, m32: 0.0 }

-- | convert parallel movement to Transform
translateToTransform :: Number -> Number -> Transform
translateToTransform x y =
  { m11: 1.0, m12: 0.0, m21: 0.0, m22: 1.0, m31: x, m32: y }

foreign import multiplyTransform
  :: Transform -> Transform -> Transform

infixr 7 multiplyTransform as |*|
