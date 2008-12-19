-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.LinearAlgebra.Static.Algorithms
-- Copyright   :  (c) Reiner Pope 2008
-- License     :  GPL-style
--
-- Maintainer  :  Reiner Pope <reiner.pope@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Common operations.
--
-----------------------------------------------------------------------------


module Numeric.LinearAlgebra.Static.Algorithms where

import Data.Complex

import Types.Data.Num
import Types.Data.Ord
import Types.Data.Bool

import Data.Packed.Static
import Data.Packed.Static.Internal

import qualified Numeric.LinearAlgebra as H

import Unsafe.Coerce

multiply :: (H.Field t) =>
            Matrix (m,n) t -> Matrix (n,p) t -> Matrix (m,p) t
multiply a b = Matrix $ H.multiply (unMatrix a) (unMatrix b)

dot :: (H.Field t) => Vector n t -> Vector n t -> t
dot a b = H.dot (unVector a) (unVector b)

linearSolve :: (H.Field t) => Matrix (m,m) t -> Matrix (m,n) t -> Matrix (m,n) t
linearSolve a b = Matrix $ H.multiply (unMatrix a) (unMatrix b)

inv :: (H.Field t) => Matrix (m,m) t -> Matrix (m,m) t
inv = Matrix . H.inv . unMatrix

pinv :: (H.Field t) => Matrix (m,n) t -> Matrix (n,m) t
pinv = Matrix . H.pinv . unMatrix

det :: (H.Field t) => Matrix (m,m) t -> t
det = H.det . unMatrix

rank :: (H.Field t) => Matrix (m,n) t -> Int
rank = H.rank . unMatrix

rcond :: (H.Field t) => Matrix (m,n) t -> Double
rcond = H.rcond . unMatrix

--------- SVD
svd :: (H.Field t) => Matrix (m,n) t ->
   (Matrix (m,m) t, Vector (Min m n) Double, Matrix (n,n) t)
svd = svdBody

-- private
svdBody m = case H.svd $ unMatrix m of
          (a,b,c) -> (Matrix a, Vector b, Matrix c)


fullSVD :: (H.Field t) => Matrix mn t
   -> (Matrix (m,m) t, Matrix (m,n) Double, Matrix (n,n) t)
fullSVD m = case H.full H.svd $ unMatrix m of
              (a,b,c) -> (Matrix a, Matrix b, Matrix c)

economySVDU :: (H.Field t) => Matrix (m,n) t -> 
   (Matrix (m,Unknown) t, 
    Vector Unknown Double, 
    Matrix (n,Unknown) t)
economySVDU m = case H.economy H.svd $ unMatrix m of
                 (a,b,c) -> (Matrix a, Vector b, Matrix c)

---- eig
eig :: H.Field t =>
       Matrix (m,m) t
    -> (Vector m (H.Complex Double), 
        Matrix (m,m) (H.Complex Double))
eig = liftEig H.eig

eigSH :: (H.Field t) => 
         Matrix (m,m) t 
      -> (Vector m Double, Matrix (m,m) t)
eigSH = liftEig H.eigSH

liftEig f m = case f $ unMatrix m of
                (a,b) -> (Vector a, Matrix b)

---- qr
qr :: (H.Field t) =>
      Matrix (m,n) t -> (Matrix (m,m) t, Matrix (m,n) t)
qr m = case H.qr $ unMatrix m of
         (a,b) -> (Matrix a, Matrix b)

--- cholesky
chol :: (H.Field t) => Matrix (m,m) t -> Matrix (m,m) t
chol = Matrix . H.chol . unMatrix

-- hessenberg
hess :: (H.Field t) =>
        Matrix (m,m) t -> (Matrix (m,m) t, Matrix (m,m) t)
hess m = case H.hess $ unMatrix m of
           (a,b) -> (Matrix a, Matrix b)

-- schur
schur :: (H.Field t) =>
         Matrix (m,m) t -> (Matrix (m,m) t, Matrix (m,m) t)
schur m = case H.schur $ unMatrix m of
            (a,b) -> (Matrix a, Matrix b)

-- lu
--- I hope these sizes is right !!
lu :: (H.Field t) =>
      Matrix (m,n) t 
   -> (Matrix (m, Min m n) t, Matrix (Min m n, n) t, Matrix (m,m) t, t)
lu m = case H.lu $ unMatrix m of
         (a,b,c,d) -> (Matrix a, Matrix b, Matrix c, d)

luPacked :: (H.Field t) =>
            Matrix (m,n) t -> (Matrix (m,n) t, [Int])
luPacked m = case H.luPacked $ unMatrix m of
               (a,b) -> (Matrix a, b)

--- is this right?
luSolve :: (H.Field t) =>
           (Matrix (m,n) t, [Int]) 
        -> Matrix (m,p) t -> Matrix (n,p) t
luSolve (Matrix lu,is) = Matrix . H.luSolve (lu,is) . unMatrix

----

expm :: (H.Field t) => Matrix (m,m) t -> Matrix (m,m) t
expm = Matrix . H.expm . unMatrix

sqrtm :: (H.Field t) => Matrix (m,m) t -> Matrix (m,m) t
sqrtm = Matrix . H.sqrtm . unMatrix

matFunc :: (H.Field t) => (Complex Double -> Complex Double)
        -> Matrix (m,m) t
        -> Matrix (m,m) (Complex Double)
matFunc f = Matrix . H.matFunc f . unMatrix

nullspacePrec :: (H.Field t) =>
                 Double -> Matrix (m,n) t -> [Vector n t]
nullspacePrec tol = map Vector . H.nullspacePrec tol . unMatrix

nullVector :: (H.Field t) =>
              Matrix (m, n) t -> Vector n t
nullVector = last . nullspacePrec 1

--- misc
ctrans :: (H.Field t) =>
          Matrix (m,n) t -> Matrix (n,m) t
ctrans = Matrix . H.ctrans . unMatrix

eps :: Double
eps = H.eps

i :: Complex Double
i = H.i

outer :: (H.Field t) =>
         Vector m t -> Vector n t -> Matrix (m,n) t
outer v w = Matrix $ H.outer (unVector v) (unVector w)

kronecker :: (H.Field t) =>
             Matrix (m,n) t -> Matrix (p,q) t -> Matrix (m :*: p, n :*: q) t
kronecker m n = Matrix $ H.kronecker (unMatrix m) (unMatrix n)

----
-- omitted: haussholder, unpackQR, unpackHess
