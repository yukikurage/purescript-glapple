module Control.Monad.State.Class.Indexed where

import Prelude

import Control.Monad.Indexed (class IxMonad)
import Data.Tuple (Tuple(..))

class IxMonad m <= IxMonadState m where
  istate :: forall x y a. (x -> Tuple a y) -> m x y a

iget :: forall m x. IxMonadState m => m x x x
iget = istate (\x -> Tuple x x)

igets :: forall m x a. IxMonadState m => (x -> a) -> m x x a
igets f = istate (\x -> Tuple (f x) x)

iput :: forall m x y. IxMonadState m => y -> m x y Unit
iput y = istate (\_ -> Tuple unit y)

imodify :: forall m x y. IxMonadState m => (x -> y) -> m x y y
imodify f = istate (\x -> Tuple (f x) (f x))

imodify_ :: forall m x y. IxMonadState m => (x -> y) -> m x y Unit
imodify_ f = istate (\x -> Tuple unit (f x))
