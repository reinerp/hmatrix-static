-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Packed.Static.Convert
-- Copyright   :  (c) Reiner Pope 2008
-- License     :  GPL-style
--
-- Maintainer  :  Reiner Pope <reiner.pope@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Conversions to other forms.
--
-----------------------------------------------------------------------------

module Data.Packed.Static.Convert where

import qualified Data.Packed.Convert as HS
import qualified Numeric.LinearAlgebra as H

import Data.Packed.Static.Shapes
import Data.Packed.Static.Internal
import Data.Packed.Static.Imports
import Data.Packed.Static.Vector
import Data.Packed.Static.Matrix

import Data.Array.MArray
import Data.Array.IArray
import Data.Array.Unboxed
import Data.Array.Storable
import Control.Monad.ST

-- | Constructs a vector from an hmatrix (dynamically-lengthed) vector.
fromHVectorU :: H.Vector t -> Vector Unknown t
fromHVectorU = wrapU

-- | Gives the underlying hmatrix representation.
toHVector :: Vector n t -> H.Vector t
toHVector = unWrap


arrayFromVector :: (Storable t) => Vector n t -> Array Int t
arrayFromVector = HS.arrayFromVector . unVector

vectorFromArray :: Storable t => Array Int t -> Vector Unknown t
vectorFromArray = Vector . HS.vectorFromArray

mArrayFromVector :: (MArray b t (ST s), Storable t) =>                    Vector n t -> ST s (b Int t)
mArrayFromVector = HS.mArrayFromVector . unVector

vectorFromMArray :: (Storable t) => Array Int t -> Vector n t
vectorFromMArray = Vector . HS.vectorFromArray

vectorToStorableArray :: (Storable t) => 
       Vector n t -> IO (StorableArray Int t)
vectorToStorableArray = HS.vectorToStorableArray . unVector

storableArrayToVector :: Storable t =>
       StorableArray Int t -> IO (Vector Unknown t)
storableArrayToVector = fmap Vector . HS.storableArrayToVector

-- | Constructs a vector from an hmatrix (dynamically-lengthed) vector.
fromHMatrixU :: H.Matrix t -> Matrix (Unknown,Unknown) t
fromHMatrixU = wrapU

-- | Gives the underlying hmatrix representation.
toHMatrix :: Matrix (m,n) t -> H.Matrix t
toHMatrix = unWrap


arrayFromMatrix :: Matrix mn Double -> UArray (Int, Int) Double
arrayFromMatrix = HS.arrayFromMatrix . unMatrix

matrixFromArray :: UArray (Int, Int) Double -> Matrix mn Double
matrixFromArray = Matrix . HS.matrixFromArray

mArrayFromMatrix :: (MArray b Double m) =>
                    Matrix mn Double -> m (b (Int, Int) Double)
mArrayFromMatrix = HS.mArrayFromMatrix . unMatrix

matrixFromMArray :: (MArray a Double (ST s)) =>
                    a (Int, Int) Double -> ST s (Matrix mn Double)
matrixFromMArray = fmap Matrix . HS.matrixFromMArray