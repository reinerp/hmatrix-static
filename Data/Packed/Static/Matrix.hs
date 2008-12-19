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
import Data.List(intercalate,transpose)
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
refineMat :: forall m n t a. Matrix (m,n) t -> (forall m' n'. (PositiveT m', PositiveT n') => Matrix (m', n') t -> a) -> a
refineMat m k = fromJust.fromJust $ reifyPositiveD (toInteger $ rows m) (\r ->
                   reifyPositiveD (toInteger $ cols m) (\c -> 
                         k (unsafeReshape m `atShape` (r,c))))

forgetRowsU :: Matrix (m,n) t -> Matrix (Unknown,n) t
forgetRowsU = unsafeReshape

forgetColsU :: Matrix (m,n) t -> Matrix (m,Unknown) t
forgetColsU = unsafeReshape

{- | Fixes a matrix's static row length.
Essentially a mechanism for partial type signatures:
you can specify the row length without specifying the 
rest of matrix's type.

@\>ident \`atRows\` d5
[$mat| 1.0, 0.0, 0.0, 0.0, 0.0;
       0.0, 1.0, 0.0, 0.0, 0.0;
       0.0, 0.0, 1.0, 0.0, 0.0;
       0.0, 0.0, 0.0, 1.0, 0.0;
       0.0, 0.0, 0.0, 0.0, 1.0 |]@
-}
atRows :: Matrix (m,n) t -> m -> Matrix (m,n) t
atRows = const

{- | Fixes a matrix's static column length.

@\> ident \`atCols\` d4
[$mat| 1.0, 0.0, 0.0, 0.0;
       0.0, 1.0, 0.0, 0.0;
       0.0, 0.0, 1.0, 0.0;
       0.0, 0.0, 0.0, 1.0 |]@
-}
atCols :: Matrix (m,n) t -> n -> Matrix (m,n) t
atCols = const

withShape :: forall m n t. (PositiveT m, PositiveT n) => (Int -> Int -> Matrix (m,n) t) -> Matrix (m,n) t
withShape f = f m n where
    m = fromIntegerT (undefined :: m)
    n = fromIntegerT (undefined :: n)

withRows :: forall m n t. (PositiveT m) => (Int -> Matrix (m,n) t) -> Matrix (m,n) t
withRows = ($ m) where
    m = fromIntegerT (undefined :: m)

withCols :: forall m n t. (PositiveT n) => (Int -> Matrix (m,n) t) -> Matrix (m,n) t
withCols = ($ n) where
    n = fromIntegerT (undefined :: n)

withSquare :: forall n t. PositiveT n => (Int -> Matrix (n,n) t) -> Matrix (n,n) t
withSquare = ($ n) where
    n = fromIntegerT (undefined :: n)

--------- to/from lists
{- | Constructs a matrix from a list. The size
in the matrix's type and the list's length must agree or
else a runtime error will be raised.

@\> matFromList [1,2,3,4,5,6] \`atShape\` (d2,d3)
[$mat| 1.0, 2.0, 3.0;
       4.0, 5.0, 6.0 |]@
-}
matFromList :: (Element t, PositiveT m, PositiveT n) => [t] -> Matrix (m,n) t
matFromList xs = withShape (\m n -> Matrix $ (m H.>< n) xs)

{- | Constructs a matrix from a list. The size
in the matrix's type and the list's length must agree or
else a runtime error will be raised.

@\> (d2 >< d3)[1,2,3,4,5,6]
[$mat| 1.0, 2.0, 3.0;
       4.0, 5.0, 6.0 |]@
-}
(><) :: (PositiveT m, PositiveT n, Element t) => m -> n -> [t] -> Matrix (m,n) t
(><) m n xs = Matrix $ ((fromIntegerT m) H.>< (fromIntegerT n)) xs

{- | Constructs a matrix from a list of lists of elements.
Each sublist must be of equal size or a runtime error
will be raised.

@\> fromListsU [[1,2,3],[4,5,6]]
[$mat| 1.0, 2.0, 3.0;
       4.0, 5.0, 6.0 |]@
-}
fromListsU :: (Element t) => [[t]] -> Matrix (Unknown,Unknown) t
fromListsU = wrapU . H.fromLists

{- | Converts a matrix to a list of its rows, each as
a list.

@\> toLists [$mat|1,2,3;4,5,6|]
[[1.0,2.0,3.0],[4.0,5.0,6.0]]@
-}
toLists :: (Element t) => Matrix (m,n) t -> [[t]]
toLists = H.toLists . unMatrix

--------- to/from row/column vectors
{- | Interprets a vector as a 1-row matrix. 

@\> asRow [$vec|1,2,3|]
[$mat| 1.0, 2.0, 3.0 |]@
-}
asRow :: (Element a) => Vector n a -> Matrix (D1,n) a
asRow = Matrix . H.asRow . unVector

{- | Interprets a vector as a 1-column matrix.

@\> asColumn [$vec|1,2,3|]
[$mat| 1.0;
       2.0;
       3.0 |]@
-}
asColumn :: (Element a) => Vector n a -> Matrix (n,D1) a
asColumn = Matrix . H.asColumn . unVector

{- | Constructs a matrix from a list of rows.

@\> fromRowsU [[$vec|1,2,3|],[$vec|4,5,6|]]
[$mat| 1.0, 2.0, 3.0;
       4.0, 5.0, 6.0 |]@
-}
fromRowsU :: (Element t) => [Vector n t] -> Matrix (Unknown,n) t
fromRowsU = Matrix . H.fromRows . map unVector

{- | Converts a matrix to a list of its rows.

@\> toRows [$mat|1,2,3;4,5,6|]
[[$vec| 1.0, 2.0, 3.0 |],[$vec| 4.0, 5.0, 6.0 |]]@
-}
toRows :: (Element t) => Matrix (m,n) t -> [Vector n t]
toRows = map Vector . H.toRows . unMatrix

{- | Constructs a matrix from a list of columns.

@\> fromColumnsU [[$vec|1,2,3|],[$vec|4,5,6|]]
[$mat| 1.0, 4.0;
       2.0, 5.0;
       3.0, 6.0 |]@
-}
fromColumnsU :: (Element t) => [Vector n t] -> Matrix (n,Unknown) t
fromColumnsU = Matrix . H.fromColumns . map unVector

{- | Converts a matrix to a list of its columns.

@\> toColumns [$mat|1,2,3;4,5,6|]
[[$vec| 1.0, 4.0 |],[$vec| 2.0, 5.0 |],[$vec| 3.0, 6.0 |]]@
-}
toColumns :: (Element t) => Matrix (m,n) t -> [Vector m t]
toColumns = map Vector . H.toColumns . unMatrix

-------- other operations
{- | Returns the number of rows of the matrix.

@\> rows [$mat|1::Double,2,3;4,5,6|]
2@
-}
rows :: Matrix (m,n) t -> Int
rows = H.rows . unMatrix

{- | Returns the number of columns of the matrix.

@\> cols [$mat|1::Double,2,3;4,5,6|]
3@
-}
cols :: Matrix (m,n) t -> Int
cols = H.cols . unMatrix

{- | Matrix transpose.

@\> trans [$mat|1,2,3;4,5,6|]
[$mat| 1.0, 4.0;
       2.0, 5.0;
       3.0, 6.0 |]@
-}
trans :: Matrix (m,n) t -> Matrix (n,m) t
trans = Matrix . H.trans . unMatrix

--- we can't disappear the Int argument... (it would require type division)
{- | Reshapes a vector into a matrix, 
with the specified number of columns. If the vector's length
is not a multiple of the required columns,
a runtime error is raised.

@\> reshapeU 3 [$vecU|1,2,3,4,5,6|]
[$mat| 1.0, 2.0, 3.0;
       4.0, 5.0, 6.0 |]@
-}
reshapeU :: (Element t) => Int -> Vector n t -> Matrix (Unknown,Unknown) t
reshapeU n = wrapU . H.reshape n . unVector

{- | Flattens a matrix into a vector.

@\> flatten [$mat|1,2,3;4,5,6|]
[$vec| 1.0, 2.0, 3.0, 4.0, 5.0, 6.0 |]@
-}
flatten :: (Element t) => Matrix (m,n) t -> Vector (m :*: n) t
flatten = Vector . H.flatten . unMatrix

{- | Indexes a matrix.

@\> [$mat|1,2,3;4,5,6|] \@\@\> (1,2)
6.0@
-}
(@@>) :: (Storable t) => Matrix (m,n) t -> (Int, Int) -> t
(@@>) = (H.@@>) . unMatrix

--- I say the size is (Unknown,Unknown) so the users don't think they all
--- have to be of the same size.
{- | Constructs a matrix from blocks.

@\> fromBlocksU [[[$matU|1,2,3;4,5,6|],  [$matU|7,8,9;10,11,12|]],
               [[$matU|11,12,13;14,15,16|],[$matU|21,22,23;24,25,26|]]]
[$mat|  1.0,  2.0,  3.0,  7.0,  8.0,  9.0;
        4.0,  5.0,  6.0, 10.0, 11.0, 12.0;
       11.0, 12.0, 13.0, 21.0, 22.0, 23.0;
       14.0, 15.0, 16.0, 24.0, 25.0, 26.0 |]@
-}
fromBlocksU :: (Element t) => [[Matrix (Unknown,Unknown) t]] -> Matrix (Unknown,Unknown) t
fromBlocksU = Matrix . H.fromBlocks . map (map unMatrix)

{- | 'replicate' for matrices.

@\> repmatU [$mat|1;2|] 2 3
[$mat| 1.0, 1.0, 1.0;
       2.0, 2.0, 2.0;
       1.0, 1.0, 1.0;
       2.0, 2.0, 2.0 |]@
-}
repmatU :: (Element t) => Matrix (m,n) t -> Int -> Int -> Matrix (Unknown,Unknown) t
repmatU m i j = Matrix $ H.repmat (unMatrix m) i j

{- | Vertically flips a matrix.

@\> flipud [$mat|1,2,3;4,5,6|]
[$mat| 4.0, 5.0, 6.0;
       1.0, 2.0, 3.0 |]@
-}
flipud :: (Element t) => Matrix (m,n) t -> Matrix (m,n) t
flipud = Matrix . H.flipud . unMatrix

{- | Horizonatlly flips a matrix.

@\> fliprl [$mat|1,2,3;4,5,6|]
[$mat| 3.0, 2.0, 1.0;
       6.0, 5.0, 4.0 |]@
-}
fliprl :: (Element t) => Matrix (m,n) t -> Matrix (m,n) t
fliprl = Matrix . H.fliprl . unMatrix

{- | Extracts a submatrix.

@\> subMatrixU (0,1) (2,2) [$mat|1,2,3,4;5,6,7,8;9,10,11,12|]
[$mat| 2.0, 3.0;
       6.0, 7.0 |]@
-}
subMatrixU :: (Element a) => (Int, Int) -> (Int, Int) -> Matrix (m,n) a -> Matrix (Unknown,Unknown) a
subMatrixU a b = Matrix . H.subMatrix a b . unMatrix

{- | Takes rows from the top of the matrix 
until the required size is reached.

@\> takeRows [$mat|1,2;3,4;5,6|] \`atRows\` d2
[$mat| 1.0, 2.0;
       3.0, 4.0 |]@
-}
takeRows :: (PositiveT m', Element t, (m' :<=: m) ~ True) => Matrix (m,n) t -> Matrix (m',n) t
takeRows a = withRows (\m -> Matrix $ H.takeRows m $ unMatrix a)

{- | Takes rows from the bottom of the matrix until
the required size is reached.

@\> dropRows [$mat|1,2;3,4;5,6|] \`atRows\` d2
[$mat| 3.0, 4.0;
       5.0, 6.0 |]@
-}
dropRows :: (PositiveT m', Element t, (m' :<=: m) ~ True) => Matrix (m,n) t -> Matrix (m',n) t
dropRows a = withRows (\m -> Matrix $ H.dropRows (rows a - m) $ unMatrix a)

{- | Takes columns from the left of the matrix until
the required size is reached.

@\> takeColumns [$mat|1,2,3;4,5,6|] \`atCols\` d2
[$mat| 1.0, 2.0;
       4.0, 5.0 |]@
-}
takeColumns :: (PositiveT n', Element t, (n' :<=: n) ~ True) => Matrix (m,n) t -> Matrix (m,n') t
takeColumns a = withCols (\n -> Matrix $ H.takeColumns n $ unMatrix a)

{- | Takes columns from the right of the matrix
until the required size is reached.

@\> dropColumns [$mat|1,2,3;4,5,6|] \`atCols\` d2
[$mat| 2.0, 3.0;
       5.0, 6.0 |]@
-}
dropColumns :: (PositiveT n', Element t, (n' :<=: n) ~ True) => Matrix (m,n) t -> Matrix (m,n') t
dropColumns a = withCols (\n -> Matrix $ H.dropColumns (cols a - n) $ unMatrix a)

{- | Extracts the given rows from a matrix.

@\> extractRowsU [1,0] [$mat|1,2;3,4;5,6|]
[$mat| 3.0, 4.0;
       1.0, 2.0 |]@
-}
extractRowsU :: (Element t) => [Int] -> Matrix (m,n) t -> Matrix (Unknown,n) t
extractRowsU is = Matrix . H.extractRows is . unMatrix

{- | Constructs the identity matrix of any given size.

@\> ident \`atRows\` d3
[$mat| 1.0, 0.0, 0.0;
       0.0, 1.0, 0.0;
       0.0, 0.0, 1.0 |]@
-}
ident :: (Element t, PositiveT n) => Matrix (n,n) t
ident = withSquare (Matrix . H.ident)

{- | Constructs a square matrix with the given vector as its diagonal.

@\> diag (linspace (1,3)) \`atRows\` d3
[$mat| 1.0, 0.0, 0.0;
       0.0, 2.0, 0.0;
       0.0, 0.0, 3.0 |]@
-}
diag :: (Element a) => Vector n a -> Matrix (n,n) a
diag = Matrix . H.diag . unVector

{- | Constructs a rectangular matrix with the given vector as its diagonal.

@\> diagRect (linspace (1,3)) \`atShape\` (d3,d4)
[$mat| 1.0, 0.0, 0.0, 0.0;
       0.0, 2.0, 0.0, 0.0;
       0.0, 0.0, 3.0, 0.0 |]

\> diagRect (linspace (1,3)) \`atShape\` (d4,d3)
[$mat| 1.0, 0.0, 0.0;
       0.0, 2.0, 0.0;
       0.0, 0.0, 3.0;
       0.0, 0.0, 0.0 |]@
-}
diagRect :: (Element a, PositiveT m, PositiveT n) => Vector (Min m n) a -> Matrix (m,n) a
diagRect v = withShape (\m n -> Matrix $ H.diagRect (unVector v) m n)

{- | Takes the diagonal from a matrix.

@\> takeDiag [$mat|1,2,3;4,5,6|]
[$vec| 1.0, 5.0 |]@
-}
takeDiag :: (Element t) => Matrix (m,n) t -> Vector (Min m n) t
takeDiag = Vector . H.takeDiag . unMatrix

{- | Operations on matrices viewed as operations on the vector of their elements 

@\> liftMatrix (+constant 2) [$mat|1,2,3;4,5,6|]
[$mat| 3.0, 4.0, 5.0;
       6.0, 7.0, 8.0 |]@
-}
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



instance (Element e, Show e) => Show (Matrix (m,n) e) where
    show m = "[$mat| " ++ format2 ", " ";\n       " m ++ " |]"

-- internal helpers

pad n str = replicate (n - length str) ' ' ++ str

padLen :: [String] -> [String]
padLen as = map (pad len) as where
    len = maximum $ map length as

padLens :: [[String]] -> [[String]]
padLens = transpose . map padLen . transpose where

format2 comma semi mat = intercalate semi . map (intercalate comma) $ padLens . map (map show) $ toLists mat
