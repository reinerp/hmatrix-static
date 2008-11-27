-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Packed.Static.Vector
-- Copyright   :  (c) Reiner Pope 2008
-- License     :  GPL-style
--
-- Maintainer  :  Reiner Pope <reiner.pope@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- Statically-dimensioned 1D vectors.
--
-----------------------------------------------------------------------------

module Data.Packed.Static.Vector(
   Vector,
   -- * Shaping
   -- | Functions manipulating a vector's (static) shape.
   refineVec,
   withDim,
   atDim,
   -- * To/from lists
   fromListU,
   toList,
   -- * Manipulation
   dim,
   (@>),
   subVectorU,
   joinU,
   constant,
   linspace,
   vectorMin,
   vectorMax,
   vectorMinIndex,
   vectorMaxIndex,
   liftVector,
   liftVector2,
 ) where

import Data.Maybe(fromJust)

import qualified Numeric.LinearAlgebra as H

import Data.Packed.Static.Shapes
import Data.Packed.Static.Imports
import Data.Packed.Static.Internal

instance ShapedContainer Vector where
    type Unwrapped Vector = H.Vector
    unWrap = unVector
    wrapU = Vector
                   
    type UnknownShape Vector = Unknown
    unsafeReshape = Vector . unVector

------ Shaping
-- | \"Reifies\" a Vector's length in types. Useful when vectors of length "Unknown"
-- need to be used for a statically-sized operations. For instance, if @v :: Vector Unknown Double@, 
-- then we can write
-- 
--    @refineVec v (\v -> forgetSize $ v + constant 5)@
-- 
-- to add a constant vector of 5s with the appropriate size.
refineVec :: forall m t a. Vector m t -> (forall n. PositiveT n => Vector n t -> a) -> a
refineVec v k = fromJust $ reifyPositiveD (toInteger $ dim v) (\n -> k (unsafeReshape v `atShape` n))

withDim :: forall n a. PositiveT n => (Int -> Vector n a) -> Vector n a
withDim f = f n where
    n = fromIntegerT (undefined :: n)

atDim :: (forall n. PositiveT n => Vector n t) -> Int -> Vector Unknown t
atDim v n | n > 0     = fromJust $ reifyPositiveD (toInteger n) (\n -> forgetShapeU $ v `atShape` n)
          | otherwise = error $ "atDim: negative vector length: " ++ show n

-------- To / from lists
-- | Constructs a vector from all the elements of a list.
fromListU :: (Storable a) => [a] -> Vector Unknown a
fromListU = Vector . H.fromList

-- | Converts to a list of elements.
toList :: (Storable a) => Vector n a -> [a]
toList = H.toList . unVector

------ Other operations
-- | Vector's length.
dim :: Vector n t -> Int
dim = H.dim . unVector

(@>) :: (Storable t) => Vector n t -> Int -> t
(@>) = (H.@>) . unVector

subVectorU :: (Storable t) => Int -> Int -> Vector n t -> Vector Unknown t
subVectorU a b = Vector . H.subVector a b . unVector

--- I say the input Size is unknown, although it can be anything,
--- so users don't think all the vectors must be of the same size.
joinU :: (Storable t) => [Vector Unknown t] -> Vector Unknown t
joinU = Vector . H.join . map unVector

constant :: (Element t, PositiveT n) => t -> Vector n t
constant a = withDim (Vector . H.constant a)

linspace :: (PositiveT n) => (Double,Double) -> Vector n Double
linspace r = withDim (\n -> Vector $ H.linspace n r) where

vectorMin :: Vector n Double -> Double
vectorMin = H.vectorMin . unVector

vectorMax :: Vector n Double -> Double
vectorMax = H.vectorMax . unVector

vectorMinIndex :: Vector n Double -> Int
vectorMinIndex = H.vectorMinIndex . unVector

vectorMaxIndex :: Vector n Double -> Int
vectorMaxIndex = H.vectorMaxIndex . unVector

liftVector :: (Storable a, Storable b) => (a -> b) -> Vector n a -> Vector n b
liftVector f = Vector . H.liftVector f . unVector

--- note: this requires they are of the same size, whereas hmatrix allows
--- different sizes; it uses the minimum size.
liftVector2 :: (Storable a, Storable b, Storable c) =>
               (a -> b -> c) -> Vector n a -> Vector n b -> Vector n c
liftVector2 f v1 v2 = Vector $ H.liftVector2 f (unVector v1) (unVector v2)