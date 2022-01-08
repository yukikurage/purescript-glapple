module Graphics.Glapple.Hooks.UseRunner where

import Prelude

import Data.Identity (Identity(..))
import Data.Tuple (snd)
import Effect.Class (class MonadEffect)
import Graphics.Glapple.Contexts (finalizeEmitterContext)
import Graphics.Glapple.Data.Context (key)
import Graphics.Glapple.Data.Emitter (Emitter, emit, newEmitter)
import Graphics.Glapple.Data.Hooks (Hooks, addContext, addContexts, runHooks, useContexts)
import Graphics.Glapple.Data.Hooks.Qualified as H
import Graphics.Glapple.Record.Maker (delete, make)
import Prim.Row (class Lacks, class Nub, class Union)

-- | 子ゲームを実行するためのHook

useRunner
  :: forall r m z
   . MonadEffect m
  => Lacks "finalizeEmitter" r
  => Union r () r
  => Nub r r
  => Hooks m (finalizeEmitter :: Emitter Unit Unit m | r) z Unit
  -> Hooks m (finalizeEmitter :: Emitter Unit Unit m | r)
       (finalizeEmitter :: Emitter Unit Unit m | r)
       (m (m Unit))
useRunner child = H.do
  contexts <- useContexts
  let
    Identity childContexts = map snd $ flip make contexts Ix.do
      delete $ key finalizeEmitterContext --finalizeEmitterは別

    launcher :: m (m Unit)
    launcher = runHooks H.do
      addContexts childContexts
      finalizeEmitter <- H.lift newEmitter
      addContext finalizeEmitterContext (finalizeEmitter :: Emitter Unit Unit m)
      child
      H.pure $ emit finalizeEmitter unit *> pure unit
  H.pure launcher
