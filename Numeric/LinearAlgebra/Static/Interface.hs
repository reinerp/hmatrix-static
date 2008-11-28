module Numeric.LinearAlgebra.Static.Interface(
    Mul(..),
    (<.>),
    (<\>),
    (H..*),
    (H.*/),
    JoinableV(..),
    JoinableH(..),
  ) where

import Data.Packed.Static
import Data.Packed.Static.Internal
import Numeric.LinearAlgebra.Static.Algorithms

import qualified Numeric.LinearAlgebra.Interface as H

class Mul a b where
    -- | Matrix-matrix, matrix-vector, or vector-matrix product.
    (<>) :: Field t => a t -> b t -> MulResult a b t
    type MulResult a b :: * -> *

instance Mul (Matrix (m,n)) (Matrix (n,p)) where
    (<>) = multiply
    type MulResult (Matrix (m,n)) (Matrix (n,p)) = Matrix (m,p)

instance Mul (Matrix (m,n)) (Vector n) where
    m <> v = Vector (unMatrix m H.<> unVector v)
    type MulResult (Matrix (m,n)) (Vector n) = Vector m

instance Mul (Vector m) (Matrix (m,n)) where
    v <> m = Vector (unVector v H.<> unMatrix m)
    type MulResult (Vector m) (Matrix (m,n)) = Vector n

-- | Dot product
(<.>) :: (Field t) => Vector n t -> Vector n t -> t
(<.>) = dot

-- | Least squares solution of a linear equation.
(<\>) :: Field t => Matrix (m,n) t -> Vector m t -> Vector n t
(<\>) m = Vector . (H.<\>) (unMatrix m) . unVector

class JoinableV a b where
    type JoinShapeV a b :: *
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