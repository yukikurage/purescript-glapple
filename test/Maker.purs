module Test.Maker
  ( test
  ) where

import Prelude

import Control.Applicative.Indexed (ipure)
import Control.Monad.Indexed.Qualified as Ix
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Graphics.Glapple.Record.Maker (delete, get, insert, make, merge, modify, nub, ref, rename, set, union)
import Test.Util (logTest)
import Type.Proxy (Proxy(..))

_x = Proxy :: Proxy "x"
_y = Proxy :: Proxy "y"
_z = Proxy :: Proxy "z"

test :: Effect Unit
test = do
  logTest (Tuple 0 init) getTest
  logTest (Tuple unit { x: 2, y: 1 }) setTest
  logTest (Tuple unit { x: 1, y: 1 }) modifyTest
  logTest (Tuple unit { x: 0, y: 1, z: 2 }) insertTest
  logTest (Tuple unit { x: 0 }) deleteTest
  logTest (Tuple unit { z: 0, y: 1 }) renameTest
  logTest (Tuple unit { x: 0, y: 2, z: 3 }) mergeTest
  logTest (Tuple init init) refTest
  logTest
    ( Tuple unit
        { x: 3, y: { x: 2, z: 3 }, z: { x: 2, y: { x: 2, z: 3 }, z: 3 } }
    )
    complexTest

init :: { x :: Int, y :: Int }
init = { x: 0, y: 1 }

getTest :: Tuple Int { x :: Int, y :: Int }
getTest = flip make init Ix.do
  x <- get _x
  ipure x

setTest :: Tuple Unit { x :: Int, y :: Int }
setTest = flip make init $ set _x 2

modifyTest :: Tuple Unit { x :: Int, y :: Int }
modifyTest = flip make init $ modify _x (_ + 1)

insertTest :: Tuple Unit { x :: Int, y :: Int, z :: Int }
insertTest = flip make init $ insert _z 2

deleteTest :: Tuple Unit { x :: Int }
deleteTest = flip make init $ delete _y

renameTest :: Tuple Unit { y :: Int, z :: Int }
renameTest = flip make init $ rename _x _z

mergeTest :: Tuple Unit { x :: Int, y :: Int, z :: Int }
mergeTest = flip make init $ merge { y: 2, z: 3 }

refTest :: Tuple { x :: Int, y :: Int } { x :: Int, y :: Int }
refTest = flip make init $ ref

complexTest :: Tuple Unit
  { x :: Int
  , y ::
      { x :: Int
      , z :: Int
      }
  , z ::
      { x :: Int
      , y ::
          { x :: Int
          , z :: Int
          }
      , z :: Int
      }
  }
complexTest = flip make init Ix.do
  set _y { x: 2, z: 3 }
  x <- get _y -- {x: 2, z: 3}
  union x -- {x: 2, y: {x: 2, z: 3}, z: 3}
  nub -- {x: 2, y: {x: 2, z: 3}, z: 3}
  y <- ref -- {x: 2, y: {x: 2, z: 3}, z: 3}
  modify _x (_ + 1) -- {x: 3, y: {x: 2, z: 3}, z: 3}
  set _z y -- {x: 3, y: {x: 2, z: 3}, z: {x: 2, y: {x: 2, z: 3}, z: 3}}
