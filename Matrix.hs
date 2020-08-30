module Matrix where
import           Complex

data Matrix = Matrix {
    mRows :: Int,
    mCols :: Int,
    mData :: [[Complex]]
} deriving Show

instance Eq Matrix where
    m1 == m2 = mData m1 == mData m2

addMatrices :: Matrix -> Matrix -> Matrix
addMatrices m1 m2
    | mRows m1 == mRows m2 && mCols m1 == mCols m2 = Matrix (mRows m1) (mCols m1) $ zipWith (zipWith addComplex) (mData m1) (mData m2)
    | otherwise = error $ "Matrix addition incompatible with matrices " ++ show (mRows m1) ++ "x" ++ show (mCols m1) ++ " and "++ show (mRows m2) ++ "x" ++ show (mCols m2)


subtractMatrices :: Matrix -> Matrix -> Matrix
subtractMatrices m1 m2 = addMatrices m1 (scalarMultiplyMatrix (Complex (-1) 0) m2)


multiplyMatrices :: Matrix -> Matrix -> Matrix
multiplyMatrices m1 m2
    | mCols m1 /= mRows m2 = error $ "Matrix multiplication incompatible with matrices " ++ show (mRows m1) ++ "x" ++ show (mCols m1) ++ " and "++ show (mRows m2) ++ "x" ++ show (mCols m2)
    | otherwise = Matrix (mRows m1) (mCols m2) $ map (foldl1 (zipWith addComplex) . zipWith (flip (map . multiplyComplex)) (mData m2)) (mData m1)


scalarMultiplyMatrix :: Complex -> Matrix -> Matrix
scalarMultiplyMatrix scalar m = Matrix (mRows m) (mCols m) $ map (map (multiplyComplex scalar)) (mData m)

tensorMatrices :: Matrix -> Matrix -> Matrix
tensorMatrices m1 m2 = Matrix (mRows m1 * mRows m2) (mCols m1 * mCols m2) [[multiplyComplex e1 e2 | e1 <- row1, e2 <- row2] | row1 <- mData m1, row2 <- mData m2]


traceMatrix :: Matrix -> Complex
traceMatrix m
    | mRows m /= mCols m = error $ "Trace is not defined for non-square matrices like " ++ show (mRows m) ++ "x" ++ show (mCols m)
    | otherwise = aux 0 (Complex 0 0) $ mData m
        where
            aux _ acc [] = acc
            aux count acc (d:dx) = if null dx then acc else aux (count+1) (addComplex acc (d!!count)) dx


transposeMatrix :: Matrix -> Matrix
transposeMatrix m = Matrix (mCols m) (mRows m) $ transpose (mData m)
    where
        transpose ([]:_) = []
        transpose d      = map head d : transpose (map tail d)

conjugateMatrix :: Matrix -> Matrix
conjugateMatrix (Matrix r c d) = Matrix r c $ map (map conjugateComplex) d

adjointMatrix :: Matrix -> Matrix
adjointMatrix m = transposeMatrix(conjugateMatrix m)

innerProductVectors :: Matrix -> Matrix -> Complex
innerProductVectors v1 v2
    | mRows v1 == mRows v2 && mCols v1 == 1 && mCols v2 == 1 = head $ head $ mData $ multiplyMatrices (adjointMatrix v1) v2
    | otherwise = error $ "Invalid matrix for inner product. Requires two equal length column vectors. Not " ++ show (mRows v1) ++ "x" ++ show (mCols v1) ++ " and "++ show (mRows v2) ++ "x" ++ show (mCols v2)


normVector :: Matrix -> Double
normVector v = sqrt $ real $ innerProductVectors v v

squareNormVector :: Matrix -> Double
squareNormVector v = real $ innerProductVectors v v

distanceVectors :: Matrix -> Matrix -> Double
distanceVectors v1 v2 = normVector (subtractMatrices v1 v2)

isHermitian :: Matrix -> Bool
isHermitian m
    | mRows m /= mCols m = False
    | otherwise = m == adjointMatrix m

isUnitary :: Matrix -> Bool
isUnitary m
    | mRows m /= mCols m = False
    | otherwise = identityMatrix (mRows m) == multiplyMatrices m (adjointMatrix m)


identityMatrix :: Int -> Matrix
identityMatrix n =
    let range = [1..n]
    in
    Matrix n n [ [if x == y then Complex 1 0 else Complex 0 0 | x <- range] | y <- range]

zeroMatrix :: Int -> Matrix
zeroMatrix n = Matrix 0 0 $ replicate n $ replicate n (Complex 0 0)

getMatrixStr :: Matrix -> String
getMatrixStr m = formatMatrix "" $ mData m
    where
    formatMatrix _ [] = ""
    formatMatrix out (d:dx)
        | null dx = out ++ show d ++ "\n"
        | otherwise = formatMatrix (out ++ show d ++ "\n") dx

listToMatrix :: Int -> Int -> [[(Double,Double)]] -> Matrix
listToMatrix r c l = Matrix r c $ map (map (\x -> Complex (fst x) (snd x))) l
