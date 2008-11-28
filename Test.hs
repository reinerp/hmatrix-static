{-# LANGUAGE QuasiQuotes, ViewPatterns #-}

module Test where

import Data.Packed.Static

foo :: (Storable t, Num t) => Vector D1 t -> Vector D3 t
foo (viewVec -> [$vec|x|]) = [$vec|5,x,7|]

bar :: (Element t) => Matrix (D2,D1) t -> Matrix (D2,D2) t
bar (viewMat -> [$mat|x;z|]) = [$mat|x,5;6,7|]
