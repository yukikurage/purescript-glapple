module Graphics.Glapple.Data.Hooks
  ( Hooks
  , addContext
  , addContexts
  , ilift
  , runHooks
  , useContext
  , useContexts
  ) where

import Prelude

import Control.Applicative.Indexed (class IxApplicative, class IxApply, ipure)
import Control.Bind.Indexed (class IxBind)
import Control.Monad.Indexed (class IxMonad)
import Control.Monad.Indexed.Qualified as Ix
import Data.Functor.Indexed (class IxFunctor)
import Data.Symbol (class IsSymbol)
import Data.Tuple (fst)
import Graphics.Glapple.Data.Context (Context)
import Graphics.Glapple.Record.Maker (Maker, disjointUnion, get, insert, ref)
import Graphics.Glapple.Record.Maker as Maker
import Prim.Row (class Cons, class Lacks, class Nub, class Union)
import Type.Proxy (Proxy(..))

newtype Hooks m x y a = Hooks (Maker m x y a)

derive newtype instance Functor m => IxFunctor (Hooks m)
derive newtype instance Monad m => IxApply (Hooks m)
derive newtype instance Monad m => IxApplicative (Hooks m)
derive newtype instance Monad m => IxBind (Hooks m)
instance Monad m => IxMonad (Hooks m)

ilift
  :: forall m x a
   . Monad m
  => m a
  -> Hooks m x x a
ilift = Hooks <<< Maker.ilift

runHooks
  :: forall m r x
   . Applicative m
  => Hooks m () r x
  -> m x
runHooks (Hooks maker) = fst <$> Maker.make maker {}

addContext
  :: forall m x y s a
   . Cons s a x y
  => Applicative m
  => Lacks s x
  => IsSymbol s
  => Context s a
  -> a
  -> Hooks m x y Unit
addContext _ a = Hooks $ insert (Proxy :: Proxy s) a

useContext
  :: forall m x z a y w s
   . Monad m
  => IsSymbol s
  => Cons s a z x
  => Cons s a w y
  => Context s a
  -> Hooks m x x a
useContext _ = Hooks $ Ix.do
  a <- get (Proxy :: Proxy s)
  ipure $ a

addContexts
  :: forall m x y z
   . Union z x y
  => Applicative m
  => Nub y y
  => Record z
  -> Hooks m x y Unit
addContexts = Hooks <<< disjointUnion

useContexts
  :: forall m r
   . Applicative m
  => Hooks m r r (Record r)
useContexts = Hooks ref
