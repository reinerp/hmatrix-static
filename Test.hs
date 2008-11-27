{-# LANGUAGE QuasiQuotes, ViewPatterns #-}

module Test where

import Data.Packed.Static.Syntax
import Data.Packed.Vector.Static
import Data.Packed.Matrix.Static

import Foreign.Storable

import Types.Data.Num.Decimal
import Types.Data.Num.Ops

foo :: Vector D1 Double -> Vector D4 Double
foo (viewVec -> [$vecD|x|]) = [$vec|5,6,7,x|]

bar = [$mat|1,5;6,7|]

