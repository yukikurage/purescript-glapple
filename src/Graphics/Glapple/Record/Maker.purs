module Graphics.Glapple.Record.Maker
  ( Maker
  , delete
  , disjointUnion
  , get
  , insert
  , make
  , merge
  , modify
  , nub
  , ref
  , rename
  , set
  , union
  ) where

import Prelude

import Control.Applicative.Indexed (class IxApplicative)
import Control.Apply.Indexed (class IxApply)
import Control.Bind.Indexed (class IxBind)
import Control.Monad.Indexed (class IxMonad)
import Data.Function.Uncurried (runFn2)
import Data.Functor.Indexed (class IxFunctor)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..))
import Prim.Row (class Cons, class Lacks, class Nub, class Union)
import Record as Record
import Record.Unsafe.Union (unsafeUnionFn)
import Unsafe.Coerce (unsafeCoerce)

foreign import copy :: forall x. Record x -> Record x
foreign import unsafeInsert :: forall a x y. String -> a -> Record x -> Record y
foreign import unsafeModify
  :: forall a b x y. String -> (a -> b) -> Record x -> Record y

foreign import unsafeDelete :: forall x y. String -> Record x -> Record y
foreign import unsafeRename
  :: forall x y. String -> String -> Record x -> Record y

newtype Maker m x y a = Maker (Record x -> m (Tuple a (Record y)))

instance Functor m => IxFunctor (Maker m) where
  imap f (Maker g) = Maker \x -> (\(Tuple a y) -> Tuple (f a) y) <$> g x

instance Monad m => IxApply (Maker m) where
  iapply (Maker f) (Maker g) = Maker \x -> do
    Tuple h y <- f x
    Tuple a z <- g y
    pure $ Tuple (h a) z

instance Monad m => IxApplicative (Maker m) where
  ipure a = Maker \x -> pure $ Tuple a x

instance Monad m => IxBind (Maker m) where
  ibind (Maker f) g = Maker \x -> do
    Tuple a y <- f x
    let
      Maker h = g a
    h y

instance Monad m => IxMonad (Maker m)

make :: forall m x y a. Maker m x y a -> Record x -> m (Tuple a (Record y))
make (Maker f) x = f $ copy x

-- | O(1)
get
  :: forall proxy p a x y m
   . IsSymbol p
  => Applicative m
  => Cons p a y x
  => proxy p
  -> Maker m x x a
get p = Maker \x -> pure $ Tuple (Record.get p x) x

-- | O(1)
set
  :: forall proxy x y z p a b m
   . IsSymbol p
  => Applicative m
  => Cons p a z x
  => Cons p b z y
  => proxy p
  -> b
  -> Maker m x y Unit
set p b = modify p (const b)

-- | O(1)
modify
  :: forall proxy p a b x y z m
   . Cons p a z x
  => Applicative m
  => Cons p b z y
  => IsSymbol p
  => proxy p
  -> (a -> b)
  -> Maker m x y Unit
modify p f = Maker $ unsafeModify (reflectSymbol p) f >>> Tuple unit >>> pure

-- | O(1)
insert
  :: forall proxy p a x y m
   . Cons p a x y
  => Applicative m
  => Lacks p x
  => IsSymbol p
  => proxy p
  -> a
  -> Maker m x y Unit
insert p a = Maker $ unsafeInsert (reflectSymbol p) a >>> Tuple unit >>> pure

-- | O(1)
delete
  :: forall proxy p a x y m
   . IsSymbol p
  => Applicative m
  => Lacks p y
  => Cons p a y x
  => proxy p
  -> Maker m x y Unit
delete p = Maker $ unsafeDelete (reflectSymbol p) >>> Tuple unit >>> pure

-- | O(1)
rename
  :: forall proxy p q a x y z m
   . IsSymbol p
  => Applicative m
  => IsSymbol q
  => Cons p a z x
  => Lacks p z
  => Cons q a z y
  => Lacks q z
  => proxy p
  -> proxy q
  -> Maker m x y Unit
rename p q = Maker $ unsafeRename (reflectSymbol p) (reflectSymbol q)
  >>> Tuple unit
  >>> pure

merge
  :: forall x y z w m
   . Union x y z
  => Applicative m
  => Nub z w
  => Record x
  -> Maker m y w Unit
merge x = Maker $ runFn2 unsafeUnionFn x >>> Tuple unit >>> pure

-- | O(|x|)
-- | where
-- | union x
union
  :: forall x y z m
   . Union x y z
  => Applicative m
  => Record x
  -> Maker m y z Unit
union x = Maker $ runFn2 unsafeUnionFn x >>> Tuple unit >>> pure

disjointUnion
  :: forall x y z m
   . Union x y z
  => Applicative m
  => Nub z z
  => Record x
  -> Maker m y z Unit
disjointUnion x = Maker $ runFn2 unsafeUnionFn x >>> Tuple unit >>> pure

-- | O(1)
nub
  :: forall x y m
   . Nub x y
  => Applicative m
  => Maker m x y Unit
nub = Maker $ unsafeCoerce >>> Tuple unit >>> pure

-- | O(N)
ref :: forall x m. Applicative m => Maker m x x (Record x)
ref = Maker \x -> pure $ Tuple (copy x) x
