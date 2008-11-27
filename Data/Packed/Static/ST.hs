-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Packed.Static.ST
-- Copyright   :  (c) Reiner Pope 2008
-- License     :  GPL-style
--
-- Maintainer  :  Reiner Pope <reiner.pope@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Manipulation of Matrix and Vector in the ST monad.
--
-----------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS -fglasgow-exts #-}

module Data.Packed.Static.ST where

import qualified Data.Packed.ST as HS
import qualified Numeric.LinearAlgebra as H

import Data.Packed.Static.Internal
import Data.Packed.Static.Imports

import Control.Monad.ST


----- vectors
newtype STVector n s t = STVector { unSTVector :: HS.STVector s t }

newVector :: forall n s t. (Element t, PositiveT n) => t -> ST s (STVector n s t) 
newVector a = fmap STVector (HS.newVector a n) where
    n = fromIntegerT (undefined :: n)

thawVector :: (Storable t) => Vector n t -> ST s (STVector n s t)
thawVector = fmap STVector . HS.thawVector . unVector

freezeVector :: (Storable t) => STVector n s1 t -> ST s2 (Vector n t)
freezeVector = fmap Vector . HS.freezeVector . unSTVector

runSTVector :: (Storable t) => (forall s. ST s (STVector n s t)) -> Vector n t
runSTVector = (Vector) . HS.runSTVector . (\v -> fmap unSTVector v) where

readVector :: (Storable t) => STVector n s t -> Int -> ST s t
readVector = HS.readVector . unSTVector

writeVector :: (Storable t) => STVector n s t -> Int -> t -> ST s ()
writeVector = HS.writeVector . unSTVector

modifyVector :: (Storable t) => STVector n s t -> Int -> (t -> t) -> ST s ()
modifyVector = HS.modifyVector . unSTVector

liftSTVector :: (Storable t) => (Vector n t -> c) -> STVector n s1 t -> ST s2 c
liftSTVector f v = HS.liftSTVector (f . Vector) (unSTVector v)

----- matrices
-- | A matrix with @m@ rows, @n@ columns.
newtype STMatrix mn s t = STMatrix { unSTMatrix :: HS.STMatrix s t }

newMatrix :: forall m n s t. (Element t, PositiveT m, PositiveT n) => t -> ST s (STMatrix (m, n) s t)
newMatrix t = fmap STMatrix $ HS.newMatrix t m n where
    m = fromIntegerT (undefined :: m)
    n = fromIntegerT (undefined :: n)

thawMatrix :: (Storable t) => Matrix (m, n) t -> ST s (STMatrix (m, n) s t)
thawMatrix = fmap STMatrix . HS.thawMatrix . unMatrix

freezeMatrix :: (Storable t) => STMatrix (m, n) s1 t -> ST s2 (Matrix (m, n) t)
freezeMatrix = fmap (Matrix) . HS.freezeMatrix . unSTMatrix

runSTMatrix :: (Storable t) => (forall s. ST s (STMatrix (m, n) s t)) -> Matrix (m, n) t
runSTMatrix = (Matrix) . HS.runSTMatrix . (\m -> fmap unSTMatrix m)

readMatrix :: (Storable t) => STMatrix (m, n) s t -> Int -> Int -> ST s t
readMatrix = HS.readMatrix . unSTMatrix

writeMatrix :: (Storable t) => STMatrix (m, n) s t -> Int -> Int -> t -> ST s ()
writeMatrix = HS.writeMatrix . unSTMatrix

modifyMatrix :: (Storable t) => STMatrix (m, n) s t -> Int -> Int -> (t -> t) -> ST s ()
modifyMatrix = HS.modifyMatrix . unSTMatrix

liftSTMatrix :: (Storable t) => (Matrix (m, n) t -> a) -> STMatrix (m, n) s1 t -> ST s2 a
liftSTMatrix f m = HS.liftSTMatrix (f . Matrix) (unSTMatrix m)

--------- unsafe
unsafeReadVector :: (Storable t) => STVector n s t -> Int -> ST s t
unsafeReadVector = HS.unsafeReadVector . unSTVector
unsafeWriteVector :: (Storable t) => STVector n s t -> Int -> t -> ST s ()
unsafeWriteVector = HS.unsafeWriteVector . unSTVector

unsafeThawVector :: (Storable t) => Vector n t -> ST s (STVector n s t)
unsafeThawVector = fmap STVector . HS.unsafeThawVector . unVector
unsafeFreezeVector :: (Storable t) => STVector n s1 t -> ST s2 (Vector n t)
unsafeFreezeVector =  fmap Vector . HS.unsafeFreezeVector . unSTVector

unsafeReadMatrix :: (Storable t) => STMatrix (m, n) s t -> Int -> Int -> ST s t
unsafeReadMatrix = HS.unsafeReadMatrix . unSTMatrix
unsafeWriteMatrix :: (Storable t) => STMatrix (m, n) s t -> Int -> Int -> t -> ST s ()
unsafeWriteMatrix = HS.unsafeWriteMatrix . unSTMatrix

unsafeThawMatrix :: (Storable t) => Matrix (m, n) t -> ST s (STMatrix (m, n) s t)
unsafeThawMatrix = fmap STMatrix . HS.unsafeThawMatrix . unMatrix
unsafeFreeMatrix :: (Storable t) => STMatrix (m, n) s1 t -> ST s2 (Matrix (m, n) t)
unsafeFreeMatrix = fmap (Matrix) . HS.unsafeFreezeMatrix . unSTMatrix


{-
--- for a later refactoring:
class STMutable a where
    data STRep a :: * -> * -> * -> *
    unsafeThaw :: Storable t => a n t -> ST s (STRep a s n t)
    unsafeFreeze :: Storable t => STRep a s n t -> ST s (a n t)
    clone :: Storable t => STRep a s n t -> ST s (STRep a s n t)

instance STMutable Vector where
    newtype STRep Vector s n t = STVector { unSTVector :: HS.STVector s t }
    unsafeThaw = fmap STVector . HS.unsafeThawVector . unVector
    unsafeFreeze = fmap Vector . HS.unsafeFreezeVector . unSTVector
    clone = undefined -- no "clone" function is available in hmatrix right now...
-}
