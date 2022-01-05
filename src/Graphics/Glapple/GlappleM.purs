module Graphics.Glapple.GlappleM
  ( Context(..)
  , GlappleM
  , addContext
  , ilift
  , runGlappleM
  , useContext
  ) where

import Prelude

import Control.Applicative.Indexed (class IxApplicative, class IxApply, ipure)
import Control.Bind.Indexed (class IxBind)
import Control.Monad.Indexed (class IxMonad)
import Control.Monad.Indexed.Qualified as Ix
import Data.Functor.Indexed (class IxFunctor)
import Data.Symbol (class IsSymbol)
import Data.Tuple (fst, snd)
import Graphics.Glapple.Record.Maker (Maker, get, insert)
import Graphics.Glapple.Record.Maker as Maker
import Prim.Row (class Cons, class Lacks)
import Type.Proxy (Proxy(..))

newtype GlappleM m x y a = GlappleM (Maker m x y a)

derive newtype instance Functor m => IxFunctor (GlappleM m)
derive newtype instance Monad m => IxApply (GlappleM m)
derive newtype instance Monad m => IxApplicative (GlappleM m)
derive newtype instance Monad m => IxBind (GlappleM m)
instance Monad m => IxMonad (GlappleM m)

data Context :: forall k1 k2. k1 -> k2 -> Type
data Context s a = Context

addContext
  :: forall m x y s a
   . Cons s a x y
  => Applicative m
  => Lacks s x
  => IsSymbol s
  => Context s a
  -> a
  -> GlappleM m x y Unit
addContext _ a = GlappleM $ insert (Proxy :: Proxy s) a

useContext
  :: forall m x z a y w s
   . Monad m
  => IsSymbol s
  => Cons s a z x
  => Cons s a w y
  => Context s a
  -> GlappleM m x x a
useContext _ = GlappleM $ Ix.do
  a <- get (Proxy :: Proxy s)
  ipure $ a

ilift
  :: forall m x a
   . Monad m
  => m a
  -> GlappleM m x x a
ilift = GlappleM <<< Maker.ilift

runGlappleM
  :: forall m r x
   . Applicative m
  => GlappleM m () r x
  -> m x
runGlappleM (GlappleM maker) = fst <$> Maker.make maker {}
