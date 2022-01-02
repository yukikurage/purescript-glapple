module Graphics.Glapple.Control.Monad.State.Indexed where

import Prelude

import Control.Applicative.Indexed (class IxApplicative, iapply, imap, ipure)
import Control.Apply.Indexed (class IxApply)
import Control.Bind.Indexed (class IxBind, ibind)
import Control.Monad.Indexed (class IxMonad)
import Control.Monad.State.Class.Indexed (class IxMonadState)
import Data.Functor.Indexed (class IxFunctor)
import Data.Tuple (Tuple(..), fst, snd)

data IxState x y a = IxState (x -> Tuple a y)

instance IxFunctor IxState where
  imap f (IxState g) = IxState \x ->
    let
      Tuple a y = g x
    in
      Tuple (f a) y

instance IxApply IxState where
  iapply (IxState f) (IxState g) = IxState \x ->
    let
      Tuple h y = f x
      Tuple a z = g y
    in
      Tuple (h a) z

instance IxApplicative IxState where
  ipure a = IxState \x -> Tuple a x

instance IxBind IxState where
  ibind (IxState f) g = IxState \x ->
    let
      Tuple a y = f x
      IxState h = g a
    in
      h y

instance IxMonad IxState

instance IxMonadState IxState where
  istate = IxState

instance Functor (IxState x y) where
  map = imap

instance Apply (IxState x x) where
  apply = iapply

instance Applicative (IxState x x) where
  pure = ipure

instance Bind (IxState x x) where
  bind = ibind

instance Monad (IxState x x)

irunState :: forall x y a. IxState x y a -> x -> Tuple a y
irunState (IxState f) = f

ievalState :: forall x y a. IxState x y a -> x -> a
ievalState (IxState f) = f >>> fst

iexecState :: forall x y a. IxState x y a -> x -> y
iexecState (IxState f) = f >>> snd

imapState
  :: forall x y a b. (Tuple a y -> Tuple b y) -> IxState x y a -> IxState x y b
imapState f (IxState g) = IxState (g >>> f)

iwithState :: forall x y z a. (y -> z) -> IxState x y a -> IxState x z a
iwithState f (IxState g) = IxState \x ->
  let
    Tuple a y = g x
  in
    Tuple a (f y)
