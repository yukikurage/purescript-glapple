module Graphics.Glapple.Data.Hooks.Qualified where

import Prelude

import Control.Applicative.Indexed (class IxApplicative, class IxApply, class IxFunctor, iapply, imap, ipure)
import Control.Bind.Indexed (class IxBind, class IxDiscard, ibind, idiscard)
import Graphics.Glapple.Data.Hooks (Hooks, ilift)

lift :: forall m x a. Monad m => m a -> Hooks m x x a
lift = ilift

map :: forall f a b x y. IxFunctor f => (a -> b) -> f x y a -> f x y b
map = imap

apply :: forall m a b x y z. IxApply m => m x y (a -> b) -> m y z a -> m x z b
apply = iapply

pure :: forall m a x. IxApplicative m => a -> m x x a
pure = ipure

bind :: forall m a b x y z. IxBind m => m x y a -> (a -> m y z b) -> m x z b
bind = ibind

discard
  :: forall a f b x y z
   . IxDiscard a
  => IxBind f
  => f x y a
  -> (a -> f y z b)
  -> f x z b
discard = idiscard
