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

newtype Maker x y a = Maker (Record x -> Tuple a (Record y))

instance IxFunctor Maker where
  imap f (Maker g) = Maker \x -> let Tuple a y = g x in Tuple (f a) y

instance IxApply Maker where
  iapply (Maker f) (Maker g) = Maker \x ->
    let
      Tuple h y = f x
      Tuple a z = g y
    in
      Tuple (h a) z

instance IxApplicative Maker where
  ipure a = Maker \x -> Tuple a x

instance IxBind Maker where
  ibind (Maker f) g = Maker \x ->
    let
      Tuple a y = f x
      Maker h = g a
    in
      h y

instance IxMonad Maker

make :: forall x y a. Maker x y a -> (Record x -> Tuple a (Record y))
make (Maker f) x = f $ copy x

-- | O(1)
get
  :: forall proxy p a x y. IsSymbol p => Cons p a y x => proxy p -> Maker x x a
get p = Maker \x -> Tuple (Record.get p x) x

-- | O(1)
set
  :: forall proxy x y z p a b
   . IsSymbol p
  => Cons p a z x
  => Cons p b z y
  => proxy p
  -> b
  -> Maker x y Unit
set p b = modify p (const b)

-- | O(1)
modify
  :: forall proxy p a b x y z
   . Cons p a z x
  => Cons p b z y
  => IsSymbol p
  => proxy p
  -> (a -> b)
  -> Maker x y Unit
modify p f = Maker $ unsafeModify (reflectSymbol p) f >>> Tuple unit

-- | O(1)
insert
  :: forall proxy p a x y
   . Cons p a x y
  => Lacks p x
  => IsSymbol p
  => proxy p
  -> a
  -> Maker x y Unit
insert p a = Maker $ unsafeInsert (reflectSymbol p) a >>> Tuple unit

-- | O(1)
delete
  :: forall proxy p a x y
   . IsSymbol p
  => Lacks p y
  => Cons p a y x
  => proxy p
  -> Maker x y Unit
delete p = Maker $ unsafeDelete (reflectSymbol p) >>> Tuple unit

-- | O(1)
rename
  :: forall proxy p q a x y z
   . IsSymbol p
  => IsSymbol q
  => Cons p a z x
  => Lacks p z
  => Cons q a z y
  => Lacks q z
  => proxy p
  -> proxy q
  -> Maker x y Unit
rename p q = Maker $ unsafeRename (reflectSymbol p) (reflectSymbol q) >>>
  Tuple unit

merge
  :: forall x y z w
   . Union x y z
  => Nub z w
  => Record x
  -> Maker y w Unit
merge x = Maker $ runFn2 unsafeUnionFn x >>> Tuple unit

-- | O(|x|)
-- | where
-- | union x
union
  :: forall x y z
   . Union x y z
  => Record x
  -> Maker y z Unit
union x = Maker $ runFn2 unsafeUnionFn x >>> Tuple unit

disjointUnion
  :: forall x y z
   . Union x y z
  => Nub z z
  => Record x
  -> Maker y z Unit
disjointUnion x = Maker $ runFn2 unsafeUnionFn x >>> Tuple unit

-- | O(1)
nub
  :: forall x y
   . Nub x y
  => Maker x y Unit
nub = Maker $ unsafeCoerce >>> Tuple unit

-- | O(N)
ref :: forall x. Maker x x (Record x)
ref = Maker \x -> Tuple (copy x) x
