module Graphics.Glapple.Hooks.UseLazy where

import Prelude

import Control.Monad.Reader (ask)
import Effect (Effect)
import Graphics.Glapple.Data.Component (Component, runComponent)

-- | Component sprite m a の処理をモナド m 内で実行できるようにする
useLazy
  :: forall sprite a
   . Component sprite a
  -> Component sprite (Effect a)
useLazy hooks = do
  internal <- ask
  let
    launch = runComponent internal hooks
  pure launch
