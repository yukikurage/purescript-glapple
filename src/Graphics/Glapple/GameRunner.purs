module Graphics.Glapple.GameRunner where

import Prelude

import Effect (Effect)
import Graphics.Canvas (CanvasElement)
import Graphics.Glapple.Data.GameId (GameId)
import Graphics.Glapple.Data.GameSpec (GameSpec, mkGameSpecM)
import Graphics.Glapple.Data.GameSpecM (CanvasSpec)
import Graphics.Glapple.Data.SpriteData (SpriteData)
import Graphics.Glapple.GameRunnerM (runChildGameM_, runGameM_)
import Graphics.Glapple.GlappleM (GlappleM)

runChildGame
  :: forall s g i o childG childI
   . GameSpec s childG childI
  -> GlappleM s g i o (GameId s childI)
runChildGame gameSpec = runChildGameM_ (mkGameSpecM gameSpec)

runGame
  :: forall sprite gameState input
   . Ord sprite
  => Number
  -> CanvasElement
  -> CanvasSpec
  -> Array (SpriteData sprite)
  -> GameSpec sprite gameState input
  -> Effect (GameId sprite input)
runGame fps canvasElement canvasSpec sprites gameSpec =
  runGameM_ fps canvasElement canvasSpec sprites (mkGameSpecM gameSpec)