module Graphics.Glapple.Hooks.UseLazy where

import Prelude

import Control.Monad.Reader (ask)
import Effect (Effect)
import Graphics.Glapple.Data.Hooks (Hooks, runHooks)

-- | Hooks sprite a の処理をモナド Effect 内で実行できるようにする
useLazy
  :: forall sprite a
   . Hooks sprite a
  -> Hooks sprite (Effect a)
useLazy hooks = do
  internal <- ask
  let
    launch = runHooks internal hooks
  pure launch
