{-# LANGUAGE QuasiQuotes, NoMonomorphismRestriction #-}

import Data.Packed.Static
import Data.Packed.Static.Convert

testQQ :: Matrix (D2,D3) Double
testQQ = [$mat|1,2,3;4,5,6|]
-- > [$mat| 1.0, 2.0, 3.0;
--          4.0, 5.0, 6.0 |]

testQQU :: Matrix (Unknown, Unknown) Double
testQQU = [$matU|1,2,3;4,5,6|]
-- > [$mat| 1.0, 2.0, 3.0;
--          4.0, 5.0, 6.0 |]

-- I haven't found a use for these yet
testRefineMat = undefined
testForgetRowsU = undefined
testForgetColsU = undefined
testWithShape = undefined
testWithRows = undefined
testWithCols = undefined
testWithSquare = undefined

testAtRows :: Matrix (D5, D5) Double
testAtRows = ident `atRows` d5
-- > [$mat| 1.0, 0.0, 0.0, 0.0, 0.0;
--          0.0, 1.0, 0.0, 0.0, 0.0;
--          0.0, 0.0, 1.0, 0.0, 0.0;
--          0.0, 0.0, 0.0, 1.0, 0.0;
--          0.0, 0.0, 0.0, 0.0, 1.0 |]

testAtCols :: Matrix (D4, D4) Double
testAtCols = ident `atCols` d4
-- > [$mat| 1.0, 0.0, 0.0, 0.0;
--          0.0, 1.0, 0.0, 0.0;
--          0.0, 0.0, 1.0, 0.0;
--          0.0, 0.0, 0.0, 1.0 |]

testCross :: Matrix (D2, D3) Double
testCross = (d2 >< d3)[1,2,3,4,5,6]
-- > [$mat| 1.0, 2.0, 3.0;
--          4.0, 5.0, 6.0 |]

testMatFromList :: Matrix (D2, D3) Double
testMatFromList = matFromList [1,2,3,4,5,6] `atShape` (d2,d3)
-- > [$mat| 1.0, 2.0, 3.0;
--          4.0, 5.0, 6.0 |]

testFromListsU :: Matrix (Unknown, Unknown) Double
testFromListsU = fromListsU [[1,2,3],[4,5,6]]
-- > [$mat| 1.0, 2.0, 3.0;
--          4.0, 5.0, 6.0 |]

testToLists :: [[Double]]
testToLists = toLists [$mat|1,2,3;4,5,6|]
-- > [[1.0,2.0,3.0],[4.0,5.0,6.0]]

testFromRowsU :: Matrix (Unknown, D3) Double
testFromRowsU = fromRowsU [[$vec|1,2,3|],[$vec|4,5,6|]]
-- > [$mat| 1.0, 2.0, 3.0;
--          4.0, 5.0, 6.0 |]

testToRows :: [Vector D3 Double]
testToRows = toRows [$mat|1,2,3;4,5,6|]
-- > [[$vec| 1.0, 2.0, 3.0 |],[$vec| 4.0, 5.0, 6.0 |]]

testFromColumnsU :: Matrix (D3, Unknown) Double
testFromColumnsU = fromColumnsU [[$vec|1,2,3|],[$vec|4,5,6|]]
-- > [$mat| 1.0, 4.0;
--          2.0, 5.0;
--          3.0, 6.0 |]

testToColumns :: [Vector D2 Double]
testToColumns = toColumns [$mat|1,2,3;4,5,6|]
-- > [[$vec| 1.0, 4.0 |],[$vec| 2.0, 5.0 |],[$vec| 3.0, 6.0 |]]

testFromBlocksU :: Matrix (Unknown, Unknown) Double
testFromBlocksU = fromBlocksU [[[$matU|1,2,3;4,5,6|],[$matU|7,8,9;10,11,12|]],
                               [[$matU|11,12,13;14,15,16|],[$matU|21,22,23;24,25,26|]]]
-- > [$mat|  1.0,  2.0,  3.0,  7.0,  8.0,  9.0;
--           4.0,  5.0,  6.0, 10.0, 11.0, 12.0;
--          11.0, 12.0, 13.0, 21.0, 22.0, 23.0;
--          14.0, 15.0, 16.0, 24.0, 25.0, 26.0 |]


testAsRow :: Matrix (D1, D3) Double
testAsRow = asRow [$vec|1,2,3|]
-- > [$mat| 1.0, 2.0, 3.0 |]

testAsColumn :: Matrix (D3, D1) Double
testAsColumn = asColumn [$vec|1,2,3|]
-- > [$mat| 1.0;
--          2.0;
--          3.0 |]


testRows :: Int
testRows = rows [$mat|1::Double,2,3;4,5,6|]
-- > 2

testCols :: Int
testCols = cols [$mat|1::Double,2,3;4,5,6|]
-- > 3

testTrans :: Matrix (D3,D2) Double
testTrans = trans [$mat|1,2,3;4,5,6|]
-- > [$mat| 1.0, 4.0;
--          2.0, 5.0;
--          3.0, 6.0 |]

testReshapeU :: Matrix (Unknown, Unknown) Double
testReshapeU = reshapeU 3 [$vecU|1,2,3,4,5,6|]
-- > [$mat| 1.0, 2.0, 3.0;
--          4.0, 5.0, 6.0 |]

testFlatten :: Vector D6 Double
testFlatten = flatten [$mat|1,2,3;4,5,6|]
-- > [$vec| 1.0, 2.0, 3.0, 4.0, 5.0, 6.0 |]

testAtAt :: Double
testAtAt = [$mat|1,2,3;4,5,6|] @@> (1,2)
-- > 6.0

testRepmatU :: Matrix (Unknown, Unknown) Double
testRepmatU = repmatU [$mat|1;2|] 2 3
-- > [$mat| 1.0, 1.0, 1.0;
--          2.0, 2.0, 2.0;
--          1.0, 1.0, 1.0;
--          2.0, 2.0, 2.0 |]

testFlipud :: Matrix (D2,D3) Double
testFlipud = flipud [$mat|1,2,3;4,5,6|]
-- > [$mat| 4.0, 5.0, 6.0;
--          1.0, 2.0, 3.0 |]

testFliprl :: Matrix (D2,D3) Double
testFliprl = fliprl [$mat|1,2,3;4,5,6|]
-- > [$mat| 3.0, 2.0, 1.0;
--          6.0, 5.0, 4.0 |]

testSubMatrixU :: Matrix (Unknown, Unknown) Double
testSubMatrixU = subMatrixU (0,1) (2,2) [$mat|1,2,3,4;5,6,7,8;9,10,11,12|]
-- > [$mat| 2.0, 3.0;
--          6.0, 7.0 |]

testTakeRows :: Matrix (D2, D2) Double
testTakeRows = takeRows [$mat|1,2;3,4;5,6|] `atRows` d2
-- > [$mat| 1.0, 2.0;
--          3.0, 4.0 |]

testDropRows :: Matrix (D2, D2) Double
testDropRows = dropRows [$mat|1,2;3,4;5,6|] `atRows` d2
-- > [$mat| 3.0, 4.0;
--          5.0, 6.0 |]

testTakeColumns :: Matrix (D2, D2) Double
testTakeColumns = takeColumns [$mat|1,2,3;4,5,6|] `atCols` d2
-- > [$mat| 1.0, 2.0;
--          4.0, 5.0 |]

testDropColumns :: Matrix (D2, D2) Double
testDropColumns = dropColumns [$mat|1,2,3;4,5,6|] `atCols` d2
-- > [$mat| 2.0, 3.0;
--          5.0, 6.0 |]

testExtractRowsU :: Matrix (Unknown, D2) Double
testExtractRowsU = extractRowsU [1,0] [$mat|1,2;3,4;5,6|]
-- > [$mat| 3.0, 4.0;
--          1.0, 2.0 |]

testIdent :: Matrix (D3, D3) Double
testIdent = ident `atRows` d3
-- > [$mat| 1.0, 0.0, 0.0;
--          0.0, 1.0, 0.0;
--          0.0, 0.0, 1.0 |]

testDiag :: Matrix (D3, D3) Double
testDiag = diag (linspace (1,3)) `atRows` d3
-- > [$mat| 1.0, 0.0, 0.0;
--          0.0, 2.0, 0.0;
--          0.0, 0.0, 3.0 |]

testDiagRect :: Matrix (D3, D4) Double
testDiagRect = diagRect (linspace (1,3)) `atShape` (d3,d4)
-- > [$mat| 1.0, 0.0, 0.0, 0.0;
--          0.0, 2.0, 0.0, 0.0;
--          0.0, 0.0, 3.0, 0.0 |]

testDiagRect2 = diagRect (linspace (1,3)) `atShape` (d4,d3)
-- > [$mat| 1.0, 0.0, 0.0;
--          0.0, 2.0, 0.0;
--          0.0, 0.0, 3.0;
--          0.0, 0.0, 0.0 |]

testTakeDiag :: Vector D2 Double
testTakeDiag = takeDiag [$mat|1,2,3;4,5,6|]
-- > [$vec| 1.0, 5.0 |]

testLiftMatrix :: Matrix (D2, D3) Double
testLiftMatrix = liftMatrix (+constant 2) [$mat|1,2,3;4,5,6|]
-- > [$mat| 3.0, 4.0, 5.0;
--          6.0, 7.0, 8.0 |]



