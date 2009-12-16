{-# LANGUAGE QuasiQuotes, NoMonomorphismRestriction #-}

import Data.Packed.Static


testQQ :: Vector D3 Double
testQQ = [$vec|1,2,3|]
-- > [$vec| 1.0, 2.0, 3.0 |]

testQQU :: Vector Unknown Double
testQQU = [$vecU|1,2,3|]
-- > [$vec| 1.0, 2.0, 3.0 |]

testAdd :: Vector D3 Double
testAdd = [$vec|2,0,15|] + [$vec|1,2,3|]
-- > [$vec| 3.0, 2.0, 18.0 |]

testSub :: Vector D3 Double
testSub = [$vec|2,0,15|] - [$vec|1,2,3|]
-- > [$vec| 1.0, -2.0, 12.0 |]

testTimes :: Vector D3 Double
testTimes = [$vec|2,0,15|] * [$vec|1,2,3|]
-- > [$vec| 2.0, 0.0, 45.0 |]

testDiv :: Vector D3 Double
testDiv = [$vec|2,0,15|] / [$vec|1,2,3|]
-- > [$vec| 2.0, 0.0, 5.0 |]

testRefineVec :: Vector Unknown Double
testRefineVec = refineVec (fromListU [2,0,15]) (\v -> forgetShapeU $ v + constant 1)
-- > [$vec| 3.0, 1.0, 16.0 |]

testAtDim :: Vector Unknown Double
testAtDim = constant 1 `atDim` 5
-- > [$vec| 1.0, 1.0, 1.0, 1.0, 1.0 |]

testAtShape :: Vector D5 Double
testAtShape = constant 1 `atShape` d5
-- > [$vec| 1.0, 1.0, 1.0, 1.0, 1.0 |]

testBuildVector :: Vector D5 Double
testBuildVector = buildVector fromIntegral `atShape` d5
-- > [$vec| 0.0, 1.0, 2.0, 3.0, 4.0 |]

testFromListU :: Vector Unknown Double
testFromListU = fromListU [1,2,3,4,5]
-- > [$vec| 1.0, 2.0, 3.0, 4.0, 5.0 |]

testToList :: [Double]
testToList = toList [$vec|1,2,3|]
-- > [1.0,2.0,3.0]

testDim :: Int
testDim = dim [$vec|1::Double,2,3|]
-- > 3

testIndex :: Double
testIndex = [$vec|1,2,3|] @> 1
-- > 2.0

testSubVectorU :: Vector Unknown Double
testSubVectorU = subVectorU 2 3 [$vec|1,2,3,4,5|]
-- > [$vec| 3.0, 4.0, 5.0 |]

testJoinU :: Vector Unknown Double
testJoinU = joinU [[$vecU|1,2,3|], [$vecU|4,5|]]
-- > [$vec| 1.0, 2.0, 3.0, 4.0, 5.0 |]

testConstant :: Vector D3 Double
testConstant = [$vec|1,2,3|] + constant 2
-- > [$vec| 3.0, 4.0, 5.0 |]

testLinspace :: Vector D4 Double
testLinspace = linspace (1,5) `atShape` d4
-- > [$vec| 1.0, 2.333333333333333, 3.6666666666666665, 5.0 |]

testVectorMin :: Double
testVectorMin = vectorMin [$vec|1,2,3|]
-- > 1.0

testVectorMax :: Double
testVectorMax = vectorMax [$vec|1,2,3|]
-- > 3.0

testVectorMinIndex :: Int
testVectorMinIndex = vectorMinIndex [$vec|1,2,3|]
-- > 0

testVectorMaxIndex :: Int
testVectorMaxIndex = vectorMaxIndex [$vec|1,2,3|]
-- > 2

testLiftVector :: Vector D3 Double
testLiftVector = (*2) `liftVector` [$vec|1,2,3|]
-- > [$vec| 2.0, 4.0, 6.0 |]

testLiftVector2 :: Vector D3 Double
testLiftVector2 = liftVector2 (+) [$vec|1,2,3|] (constant 3)
-- > [$vec| 4.0, 5.0, 6.0 |]