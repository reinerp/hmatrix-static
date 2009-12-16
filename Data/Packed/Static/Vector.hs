-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Packed.Static.Vector
-- Copyright   :  (c) Reiner Pope 2008
-- License     :  GPL-style
--
-- Maintainer  :  Reiner Pope <reiner.pope@gmail.com>
-- Stability   :  experimental
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
   atDim,
   atShape,
   -- * Construction by index
   buildVector,
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
import Data.List(intercalate)

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

{- | Sets an arbitrary-length vector to a specific value.

@\> constant 1 `atDim` 5
[$vec| 1.0, 1.0, 1.0, 1.0, 1.0 |]@
-}
atDim :: (forall n. PositiveT n => Vector n t) -> Int -> Vector Unknown t
atDim v n | n > 0     = fromJust $ reifyPositiveD (toInteger n) (\n -> forgetShapeU $ v `atShape` n)
          | otherwise = error $ "atDim: negative vector length: " ++ show n

------- By-index construction
{- | Builds a vector given a function from indices. Indexing is 0-based.

@\> buildVector fromIntegral \`atShape\` d5
[$vec| 0.0, 1.0, 2.0, 3.0, 4.0 |]@
-}
buildVector :: (PositiveT n, Element a) => (Int -> a) -> Vector n a
buildVector f = withDim (\len -> Vector $ H.buildVector len f)


-------- To / from lists
{- | Constructs a vector from all the elements of a list.

@\> fromListU [1,2,3,4,5]
[$vec| 1.0, 2.0, 3.0, 4.0, 5.0 |]@
-}
fromListU :: (Storable a) => [a] -> Vector Unknown a
fromListU = Vector . H.fromList

{- | Converts to a list of elements.

@\> toList [$vec|1,2,3|]
[1.0,2.0,3.0]@
-}
toList :: (Storable a) => Vector n a -> [a]
toList = H.toList . unVector

------ Other operations
{- | Vector's length.

@\> dim [$vec|1::Double,2,3|]
3@
-}
dim :: Vector n t -> Int
dim = H.dim . unVector

{- | Indexes a vector.

@\> [$vec|1,2,3|] \@\> 1
2.0@
-}
(@>) :: (Storable t) => Vector n t -> Int -> t
(@>) = (H.@>) . unVector

{- | Extracts a subvector.

@\> subVectorU 2 3 [$vec|1,2,3,4,5|]
[$vec| 3.0, 4.0, 5.0 |]@
-}
subVectorU :: (Storable t) => 
              Int -- ^ Initial index
           -> Int -- ^ Length of resultant vector
           -> Vector n t
           -> Vector Unknown t
subVectorU a b = Vector . H.subVector a b . unVector

--- I say the input Size is unknown, although it can be anything,
--- so users don't think all the vectors must be of the same size.

{- | Joins each vector in the list.

@\> joinU [[$vecU|1,2,3|], [$vecU|4,5|]]
[$vec| 1.0, 2.0, 3.0, 4.0, 5.0 |]@
-}
joinU :: (Storable t) => [Vector Unknown t] -> Vector Unknown t
joinU = Vector . H.join . map unVector

{- | Creates a constant vector of any length. The length is
determined by the type.

@\> [$vec|1,2,3|] + constant 2
[$vec| 3.0, 4.0, 5.0 |]@
-}
constant :: (Element t, PositiveT n) => t -> Vector n t
constant a = withDim (Vector . H.constant a)

{- | Creates a vector of arbitrary length whose
components range linearly from a to b. The vector's
length is determined by its type.

@\> linspace (1,5) `atShape` d4
[$vec| 1.0, 2.333333333333333, 3.6666666666666665, 5.0 |]@
-}
linspace :: (PositiveT n) => (Double,Double) -> Vector n Double
linspace r = withDim (\n -> Vector $ H.linspace n r) where

{- | Gives the vector's minimum entry.

@\> vectorMin [$vec|1,2,3|]
1.0@
-}
vectorMin :: Vector n Double -> Double
vectorMin = H.vectorMin . unVector

{- | Gives the vector's maximum entry.

@\> vectorMax [$vec|1,2,3|]
3.0@
-}
vectorMax :: Vector n Double -> Double
vectorMax = H.vectorMax . unVector

{- | Gives the index of a vector's minimum entry.
@\> vectorMinIndex [$vec|1,2,3|]
0@
-}
vectorMinIndex :: Vector n Double -> Int
vectorMinIndex = H.vectorMinIndex . unVector

{- | Gives the index of a vector's maximum entry. 

@\> vectorMaxIndex [$vec|1,2,3|]
2@
-}
vectorMaxIndex :: Vector n Double -> Int
vectorMaxIndex = H.vectorMaxIndex . unVector

{- | 'map' for vectors.

@\> (*2) `liftVector` [$vec|1,2,3|]
[$vec| 2.0, 4.0, 6.0 |]@
-}
liftVector :: (Storable a, Storable b) => (a -> b) -> Vector n a -> Vector n b
liftVector f = Vector . H.mapVector f . unVector

--- note: this requires they are of the same size, whereas hmatrix allows
--- different sizes; it uses the minimum size.
{- | 'zipWith' for vectors. 

@\> liftVector2 (+) [$vec|1,2,3|] (constant 3)
[$vec| 4.0, 5.0, 6.0 |]@
-}
liftVector2 :: (Storable a, Storable b, Storable c) =>
               (a -> b -> c) -> Vector n a -> Vector n b -> Vector n c
liftVector2 f v1 v2 = Vector $ H.zipVector f (unVector v1) (unVector v2)

instance (Storable e, Show e) => Show (Vector n e) where
    show v = "[$vec| " ++ intercalate ", " (map show $ toList v) ++ " |]"

