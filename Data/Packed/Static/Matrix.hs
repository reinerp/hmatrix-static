-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Packed.Static.Matrix
-- Copyright   :  (c) Reiner Pope 2008
-- License     :  GPL-style
--
-- Maintainer  :  Reiner Pope <reiner.pope@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Statically-dimensioned 2D matrices.
--
-----------------------------------------------------------------------------

{-# OPTIONS -fglasgow-exts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Data.Packed.Static.Matrix(
  Matrix,
  -- * Shaping
  refineMat,
  forgetRowsU,
  forgetColsU,
  atRows,
  atCols,
  withShape,
  withRows,
  withCols,
  withSquare,
  -- * To/from lists
  (><),
  matFromList,
  fromListsU,
  toLists,
  -- * To/from rows/column vectors
  fromRowsU,
  toRows,
  fromColumnsU,
  toColumns,
  fromBlocksU,
  asRow,
  asColumn,
  -- * Other operations
  rows,
  cols,
  trans,
  reshapeU,
  flatten,
  (@@>),
  repmatU,
  flipud,
  fliprl,
  subMatrixU,
  takeRows,
  dropRows,
  takeColumns,
  dropColumns,
  extractRowsU,
  ident,
  diag,
  diagRect,
  takeDiag, 
  liftMatrix,
  -- * Conversions / file input
  format,
  readMatrix,
  fromFile,
  fromArray2D,
 ) where

import Data.Array

import Data.Maybe(fromJust)
import qualified Numeric.LinearAlgebra as H

import Data.Packed.Static.Shapes
import Data.Packed.Static.Imports
import Data.Packed.Static.Internal

instance ShapedContainer Matrix where
    type Unwrapped Matrix = H.Matrix
    unWrap = unMatrix
    wrapU = Matrix

    type UnknownShape Matrix = (Unknown, Unknown)
    unsafeReshape = Matrix . unMatrix

------ Shaping
-- TODO: turn IntegerT constraint into PositiveT constraint
refineMat :: forall m n t a. Matrix (m,n) t -> (forall m' n'. (PositiveT m', PositiveT n') => Matrix (m', n') t -> a) -> a
refineMat m k = fromJust.fromJust $ reifyPositiveD (toInteger $ rows m) (\r ->
                   reifyPositiveD (toInteger $ cols m) (\c -> 
                         k (unsafeReshape m `atShape` (r,c))))

forgetRowsU :: Matrix (m,n) t -> Matrix (Unknown,n) t
forgetRowsU = unsafeReshape

forgetColsU :: Matrix (m,n) t -> Matrix (m,Unknown) t
forgetColsU = unsafeReshape

atRows :: Matrix (m,n) t -> m -> Matrix (m,n) t
atRows = const

atCols :: Matrix (m,n) t -> n -> Matrix (m,n) t
atCols = const

withShape :: forall m n t. (PositiveT m, PositiveT n) => (Int -> Int -> Matrix (m,n) t) -> Matrix (m,n) t
withShape f = f m n where
    m = fromIntegerT (undefined :: m)
    n = fromIntegerT (undefined :: n)

withRows :: forall m n t. (PositiveT m) => (Int -> Matrix (m,n) t) -> Matrix (m,n) t
withRows = ($m) where
    m = fromIntegerT (undefined :: m)

withCols :: forall m n t. (PositiveT n) => (Int -> Matrix (m,n) t) -> Matrix (m,n) t
withCols = ($n) where
    n = fromIntegerT (undefined :: n)

withSquare :: forall n t. PositiveT n => (Int -> Matrix (n,n) t) -> Matrix (n,n) t
withSquare = ($n) where
    n = fromIntegerT (undefined :: n)

--------- to/from lists
matFromList :: (Element t, PositiveT m, PositiveT n) => [t] -> Matrix (m,n) t
matFromList xs = withShape (\m n -> Matrix $ (m H.>< n) xs)

(><) :: (PositiveT m, PositiveT n, Element t) => m -> n -> [t] -> Matrix (m,n) t
(><) m n xs = Matrix $ ((fromIntegerT m) H.>< (fromIntegerT n)) xs

fromListsU :: (Element t) => [[t]] -> Matrix (Unknown,Unknown) t
fromListsU = wrapU . H.fromLists

toLists :: (Element t) => Matrix (m,n) t -> [[t]]
toLists = H.toLists . unMatrix

--------- to/from row/column vectors
asRow :: (Element a) => Vector n a -> Matrix (D1,n) a
asRow = Matrix . H.asRow . unVector

asColumn :: (Element a) => Vector n a -> Matrix (n,D1) a
asColumn = Matrix . H.asColumn . unVector

fromRowsU :: (Element t) => [Vector n t] -> Matrix (Unknown,n) t
fromRowsU = Matrix . H.fromRows . map unVector

toRows :: (Element t) => Matrix (m,n) t -> [Vector n t]
toRows = map Vector . H.toRows . unMatrix

fromColumnsU :: (Element t) => [Vector n t] -> Matrix (n,Unknown) t
fromColumnsU = Matrix . H.fromColumns . map unVector

toColumns :: (Element t) => Matrix (m,n) t -> [Vector m t]
toColumns = map Vector . H.toColumns . unMatrix

-------- other operations
rows :: Matrix (m,n) t -> Int
rows = H.rows . unMatrix

cols :: Matrix (m,n) t -> Int
cols = H.cols . unMatrix

trans :: Matrix (m,n) t -> Matrix (n,m) t
trans = Matrix . H.trans . unMatrix

--- we can't disappear the Int argument... (it would require type division)
reshapeU :: (Element t) => Int -> Vector n t -> Matrix (Unknown,Unknown) t
reshapeU n = wrapU . H.reshape n . unVector

flatten :: (Element t) => Matrix (m,n) t -> Vector (m :*: n) t
flatten = Vector . H.flatten . unMatrix

(@@>) :: (Storable t) => Matrix (m,n) t -> (Int, Int) -> t
(@@>) = (H.@@>) . unMatrix

--- I say the size is (Unknown,Unknown) so the users don't think they all
--- have to be of the same size.
fromBlocksU :: (Element t) => [[Matrix (Unknown,Unknown) t]] -> Matrix (Unknown,Unknown) t
fromBlocksU = Matrix . H.fromBlocks . map (map unMatrix)

repmatU :: (Element t) => Matrix (m,n) t -> Int -> Int -> Matrix (Unknown,Unknown) t
repmatU m i j = Matrix $ H.repmat (unMatrix m) i j

flipud :: (Element t) => Matrix (m,n) t -> Matrix (m,n) t
flipud = Matrix . H.flipud . unMatrix

fliprl :: (Element t) => Matrix (m,n) t -> Matrix (m,n) t
fliprl = Matrix . H.fliprl . unMatrix

subMatrixU :: (Element a) => (Int, Int) -> (Int, Int) -> Matrix (m,n) a -> Matrix (Unknown,Unknown) a
subMatrixU a b = Matrix . H.subMatrix a b . unMatrix

takeRows :: (PositiveT m', Element t, (m' :<=: m) ~ True) => Matrix (m,n) t -> Matrix (m',n) t
takeRows a = withRows (\m -> Matrix $ H.takeRows m $ unMatrix a)

dropRows :: (PositiveT m', Element t, (m' :<=: m) ~ True) => Matrix (m,n) t -> Matrix (m',n) t
dropRows a = withRows (\m -> Matrix $ H.dropRows (rows a - m) $ unMatrix a)

takeColumns :: (PositiveT n', Element t, (n' :<=: n) ~ True) => Matrix (m,n) t -> Matrix (m,n') t
takeColumns a = withCols (\n -> Matrix $ H.takeColumns n $ unMatrix a)

dropColumns :: (PositiveT n', Element t, (n' :<=: n) ~ True) => Matrix (m,n) t -> Matrix (m,n') t
dropColumns a = withCols (\n -> Matrix $ H.dropColumns (cols a - n) $ unMatrix a)

extractRowsU :: (Element t) => [Int] -> Matrix (m,n) t -> Matrix (Unknown,n) t
extractRowsU is = Matrix . H.extractRows is . unMatrix

ident :: (Element t, PositiveT n) => Matrix (n,n) t
ident = withSquare (Matrix . H.ident)

diag :: (Element a) => Vector n a -> Matrix (n,n) a
diag = Matrix . H.diag . unVector

diagRect :: (Element a, PositiveT m, PositiveT n) => Vector (Min m n) a -> Matrix (m,n) a
diagRect v = withShape (\m n -> Matrix $ H.diagRect (unVector v) m n)

takeDiag :: (Element t) => Matrix (m,n) t -> Vector (Min m n) t
takeDiag = Vector . H.takeDiag . unMatrix

liftMatrix :: (Element a, Element b) => (Vector (m :*: n) a -> Vector (m :*: n) b) -> Matrix (m,n) a -> Matrix (m,n) b
liftMatrix f = Matrix . H.liftMatrix (unVector . f . Vector) . unMatrix

liftMatrix2 :: (Element t, Element a, Element b) =>
               (Vector (m:*:n) a -> Vector (m:*:n) b -> Vector (m:*:n) t)
               -> Matrix (m,n) a
               -> Matrix (m,n) b
               -> Matrix (m,n) t
liftMatrix2 f a b = Matrix $ H.liftMatrix2 (\v w -> unVector $ f (Vector v) (Vector w)) (unMatrix a) (unMatrix b)

format :: (Element t) => String -> (t -> String) -> Matrix (m,n) t -> String
format s f = H.format s f . unMatrix

readMatrix :: String -> Matrix (Unknown,Unknown) Double
readMatrix = wrapU . H.readMatrix

fromFile :: FilePath -> (Int, Int) -> IO (Matrix (Unknown,Unknown) Double)
fromFile f s = fmap wrapU $ H.fromFile f s

fromArray2D :: (Element t) => Array (Int, Int) t -> Matrix (Unknown,Unknown) t
fromArray2D = wrapU . H.fromArray2D

