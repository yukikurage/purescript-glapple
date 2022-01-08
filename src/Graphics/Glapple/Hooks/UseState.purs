module Graphics.Glapple.Hooks.UseState where

import Prelude

import Data.Tuple.Nested (type (/\), (/\))
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (new, read, write)
import Graphics.Glapple.Data.Hooks (Hooks)
import Graphics.Glapple.Data.Hooks.Qualified as H

useState
  :: forall r m a
   . MonadEffect m
  => a
  -> Hooks m r r ((m a) /\ (a -> m Unit))
useState x = H.do
  ref <- H.lift $ liftEffect $ new x
  H.pure $ (liftEffect $ read ref) /\ (\y -> liftEffect $ write y ref)
