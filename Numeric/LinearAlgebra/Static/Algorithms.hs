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

module Numeric.LinearAlgebra.Static.Algorithms(
    -- * Type hints
    matT,
    vecT,
    doubleT,
    complexT,
    -- * Multiplication                                           
    Mul(..),
    (<.>),
    -- * Concatenating
    (<->),
    (<|>),
    -- * Solving \/ inverting
    (<\>),
    linearSolve,
    inv,
    pinv,
    -- * Determinant \/ rank \/ condition number
    det,
    rank,
    rcond,
    -- * Eigensystems
    eig,
    eigSH,

    -- * Factorisations
    -- ** SVD
    svd,
    fullSVD,
    economySVDU,
    -- ** QR
    qr,
    -- ** Cholesky
    chol,
    -- ** Hessenberg
    hess,
    -- ** Schur
    schur,
    -- ** LU
    lu,
    luPacked,
    luSolve,

    -- * Matrix functions
    expm,
    sqrtm,
    matFunc,
    
    -- * Nullspace
    nullspacePrec,
    nullVector,

    -- * Norms
    pnorm,
    H.NormType(..),

    -- * Misc
    ctrans,
    eps,
    i,
    outer,
    kronecker,
 ) where

import Data.Complex

import Types.Data.Num
import Types.Data.Ord
import Types.Data.Bool

import Data.Packed.Static
import Data.Packed.Static.Internal

import qualified Numeric.LinearAlgebra as H
import           Numeric.LinearAlgebra(pnorm)

matT :: Matrix s t -> a
matT = const undefined

vecT :: Vector s t -> a
vecT = const undefined

doubleT :: a s Double -> x
doubleT = const undefined

complexT :: a s (Complex Double) -> x
complexT = const undefined

class Mul a b where
    -- | Overloaded matrix-matrix, matrix-vector, or vector-matrix product.
    --   The instances have type equalities to improve the quality of
    --   type inference.
    (<>) :: Field t => a t -> b t -> MulResult a b t
    type MulResult a b :: * -> *

instance (n ~ n') => Mul (Matrix (m,n)) (Matrix (n',p)) where
    a <> b = Matrix $ H.multiply (unMatrix a) (unMatrix b)
    type MulResult (Matrix (m,n)) (Matrix (n',p)) = Matrix (m,p)

instance (n ~ n') => Mul (Matrix (m,n)) (Vector n') where
    m <> v = Vector (unMatrix m H.<> unVector v)
    type MulResult (Matrix (m,n)) (Vector n') = Vector m

instance (m ~ m') => Mul (Vector m) (Matrix (m',n)) where
    v <> m = Vector (unVector v H.<> unMatrix m)
    type MulResult (Vector m) (Matrix (m',n)) = Vector n

-- | Dot product
(<.>) :: (Field t) => Vector n t -> Vector n t -> t
a <.> b = H.dot (unVector a) (unVector b)

class JoinableV a b where
    type JoinShapeV a b :: *
    -- | Overloaded matrix-matrix, matrix-vector, vector-matrix, or vector-vector
    --   vertical concatenation. The instances have type equalities
    --   to improve the quality of type inference.
    (<->) :: Element t => a t -> b t -> Matrix (JoinShapeV a b) t

instance JoinableV (Matrix (m,n)) (Matrix (p,n)) where
    type JoinShapeV (Matrix (m,n)) (Matrix (p,n)) = (m :+: p, n)
    m <-> n = Matrix (unMatrix m H.<-> unMatrix n)

instance JoinableV (Matrix (m,n)) (Vector n) where
    type JoinShapeV (Matrix (m,n)) (Vector n) = (m :+: D1, n)
    m <-> v = m <-> asRow v

instance JoinableV (Vector n) (Matrix (m,n)) where
    type JoinShapeV (Vector n) (Matrix (m,n)) = (D1 :+: m, n)
    v <-> m = asRow v <-> m

instance JoinableV (Vector n) (Vector n) where
    type JoinShapeV (Vector n) (Vector n) = (D2, n)
    v <-> w = asRow v <-> w

class JoinableH a b where
    type JoinShapeH a b :: *
    -- | Overloaded matrix-matrix, matrix-vector, vector-matrix,
    --   or vector-vector horizontal concatenation. The
    --   instances have type equalities to
    --   improve the quality of type inference.
    (<|>) :: Element t => a t -> b t -> Matrix (JoinShapeH a b) t

instance JoinableH (Matrix (m,n)) (Matrix (m,p)) where
    type JoinShapeH (Matrix (m,n)) (Matrix (m,p)) = (m,n:+:p)
    m <|> n = Matrix (unMatrix m H.<|> unMatrix n)

instance JoinableH (Matrix (m,n)) (Vector m) where
    type JoinShapeH (Matrix (m,n)) (Vector m) = (m,n:+:D1)
    m <|> v = m <|> asColumn v

instance JoinableH (Vector m) (Matrix (m,n)) where
    type JoinShapeH (Vector m) (Matrix (m,n)) = (m,D1 :+: n)
    v <|> m = asColumn v <|> m

instance JoinableH (Vector m) (Vector m) where
    type JoinShapeH (Vector m) (Vector m) = (m,D2)
    v <|> w = asColumn v <|> w

-- | Least squares solution of a linear equation.
(<\>) :: Field t => Matrix (m,n) t -> Vector m t -> Vector n t
(<\>) m = Vector . (H.<\>) (unMatrix m) . unVector

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

