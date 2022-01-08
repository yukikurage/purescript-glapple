module Graphics.Glapple.Data.Context where

import Type.Proxy (Proxy(..))

data Context :: forall k1 k2. k1 -> k2 -> Type
data Context s a = Context

key :: forall s a. Context s a -> Proxy s
key _ = Proxy

value :: forall s a. Context s a -> Proxy s
value _ = Proxy
