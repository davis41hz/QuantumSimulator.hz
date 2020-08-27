-- COMPLEX ARITHMETIC
data Complex = Complex {
    real      :: Double,
    imaginary :: Double
}

instance Show Complex where
    show (Complex r c) = show r ++ "+" ++ show c ++ "i"

instance Eq Complex where
    c1 == c2 = abs ((real c1) - (real c2)) < 0.0000000000001 && abs ((imaginary c1) - (imaginary c2)) < 0.00000000000001

data Polar = Polar {
    magnitude :: Double,
    angle     :: Double
} deriving Show

addComplex :: Complex -> Complex -> Complex
addComplex (Complex r1 i1) (Complex r2 i2) = Complex (r1 + r2) (i1 + i2)

subtractComplex :: Complex -> Complex -> Complex
subtractComplex (Complex r1 i1) (Complex r2 i2) = Complex (r1 - r2) (i1 - i2)

multiplyComplex :: Complex -> Complex -> Complex
multiplyComplex (Complex r1 i1) (Complex r2 i2) = Complex (r1*r2 - i1*i2) (r1*i2 + i1*r2)

divideComplex :: Complex -> Complex -> Complex
divideComplex (Complex _ _) (Complex 0 0) = error "Division by zero"
divideComplex (Complex r1 i1) (Complex r2 i2) =
    let denominator = r2*r2 + i2*i2
    in Complex ((r1*r2 + i1*i2)/denominator) ((i1*r2 - r1*i2)/denominator)

conjugateComplex :: Complex -> Complex
conjugateComplex (Complex r i) = Complex r (-i)

modulusComplex :: Complex -> Double
modulusComplex (Complex r i) = sqrt (r*r + i*i)

cartesianToPolar :: Complex -> Polar
cartesianToPolar (Complex r i)
    | r > 0 = Polar (modulusComplex (Complex r i)) (atan i/r)
    | r < 0 = Polar (modulusComplex (Complex r i)) (atan i/r + pi)
    | otherwise = Polar (abs i) (signum i * 0.5 * pi)

polarToCartesian :: Polar -> Complex
polarToCartesian (Polar m a) = Complex (m * cos a) (m * sin a)

multiplyPolar :: Polar -> Polar -> Polar
multiplyPolar (Polar m1 a1) (Polar m2 a2) = Polar (m1 * m2) (a1 + a2)

dividePolar :: Polar -> Polar -> Polar
dividePolar (Polar _ _) (Polar 0 _)     = error "Division by zero"
dividePolar (Polar m1 a1) (Polar m2 a2) = Polar (m1 / m2) (a1 - a2)

powerPolar :: Polar -> Int -> Polar
powerPolar (Polar m a) e
    | e >= 1 || e == 0 = Polar (m^e) (a * fromIntegral e)
    | otherwise = error "Only positive integers allowed in the exponent"

-- MATRICES

data Matrix = Matrix {
    mRows :: Int,
    mCols :: Int,
    mData :: [[Complex]]
} deriving Show

instance Eq Matrix where
    m1 == m2 = (mData m1) == (mData m2)

getMatrixStr :: Matrix -> String
getMatrixStr m = formatMatrix "" $ mData m
    where
    formatMatrix out (d:dx)
        | null dx = out ++ show d ++ "\n"
        | otherwise = formatMatrix (out ++ show d ++ "\n") dx

addMatrices :: Matrix -> Matrix -> Matrix
addMatrices m1 m2
    | (mRows m1) == (mRows m2) && (mCols m1) == (mCols m2) = Matrix (mRows m1) (mCols m1) $ zipWith (\e1 e2 -> zipWith addComplex e1 e2) (mData m1) (mData m2)
    | otherwise = error $ "Matrix addition incompatible with matrices " ++ (show $ mRows m1) ++ "x" ++ (show $ mCols m1) ++ " and "++ (show $ mRows m2) ++ "x" ++ (show $ mCols m2)


subtractMatrices :: Matrix -> Matrix -> Matrix
subtractMatrices m1 m2 = addMatrices m1 (scalarMultiplyMatrix (Complex (-1) 0) m2)


scalarMultiplyMatrix :: Complex -> Matrix -> Matrix
scalarMultiplyMatrix scalar m = Matrix (mRows m) (mCols m) $ map (map (multiplyComplex scalar)) (mData m)

traceMatrix :: Matrix -> Complex
traceMatrix m
    | (mRows m) /= (mCols m) = error $ "Trace is not defined for non-square matrices like " ++ (show $ mRows m) ++ "x" ++ (show $ mCols m)
    | otherwise = aux 0 (Complex 0 0) $ mData m
        where
            aux count acc (d:dx) = if null dx then acc else aux (count+1) (addComplex acc (d!!count)) dx -- FIX


multiplyMatrices :: Matrix -> Matrix -> Matrix
multiplyMatrices m1 m2
    | (mCols m1) /= (mRows m2) = error $ "Matrix multiplication incompatible with matrices " ++ (show $ mRows m1) ++ "x" ++ (show $ mCols m1) ++ " and "++ (show $ mRows m2) ++ "x" ++ (show $ mCols m2)
    | otherwise = Matrix (mRows m1) (mCols m2) $ map ((\x -> foldl1 (zipWith addComplex) x) . zipWith (flip (map . multiplyComplex)) (mData m2)) (mData m1)

transposeMatrix :: Matrix -> Matrix
transposeMatrix m = Matrix (mCols m) (mRows m) $ transpose (mData m)
    where
        transpose ([]:_) = []
        transpose d      = (map head d) : transpose (map tail d)

conjugateMatrix :: Matrix -> Matrix
conjugateMatrix (Matrix r c d) = Matrix r c $ map (map (conjugateComplex)) d

adjointMatrix :: Matrix -> Matrix
adjointMatrix m = transposeMatrix(conjugateMatrix m)

innerProductVectors :: Matrix -> Matrix -> Complex
innerProductVectors v1 v2
    | (mRows v1) == (mRows v2) && (mCols v1) == 1 && (mCols v2) == 1 = foldl addComplex (Complex 0 0) $ zipWith (\x1 x2 -> multiplyComplex (x1!!0) (conjugateComplex (x2!!0))) (mData v1) (mData v2)
    | otherwise = error $ "Invalid matrix for inner product. Requires two equal length column vectors. Not " ++ (show $ mRows v1) ++ "x" ++ (show $ mCols v1) ++ " and "++ (show $ mRows v2) ++ "x" ++ (show $ mCols v2)


normVector :: Matrix -> Double
normVector v = sqrt $ real $ innerProductVectors v v

distanceVectors :: Matrix -> Matrix -> Double
distanceVectors v1 v2 = normVector (subtractMatrices v1 v2)

isHermitian :: Matrix -> Bool
isHermitian m
    | (mRows m) /= (mCols m) = False
    | otherwise = m == adjointMatrix m

isUnitary :: Matrix -> Bool
isUnitary m
    | (mRows m) /= (mCols m) = False
    | otherwise = identityMatrix (mRows m) == (multiplyMatrices m $ adjointMatrix m)

tensorMatrices :: Matrix -> Matrix -> Matrix
tensorMatrices m1 m2 =
    let
    doThing r1 r2 = [multiplyComplex e1 e2 | e1 <- r1, e2 <- r2]
    in
    Matrix (mRows m1 * mRows m2) (mCols m1 * mCols m2) [doThing row1 row2 | row1 <- (mData m1), row2 <- (mData m2)]

identityMatrix :: Int -> Matrix
identityMatrix n =
    let range = [1..n]
    in
    Matrix n n [ [if x == y then (Complex 1 0) else (Complex 0 0) | x <- range] | y <- range]

listToMatrix :: Int -> Int -> [[(Double,Double)]] -> Matrix
listToMatrix r c l = Matrix r c $ map (map (\x -> Complex (fst x) (snd x))) l

main :: IO ()
main =
    let m1 = listToMatrix 3 3 [ [(3,2), (5,-1), (0,2)],
                                [(0,0), (12,0), (6,-3)],
                                [(2,0), (4,4), (9,3)] ]
        m2 = listToMatrix 3 3 [ [(1,0), (3,4), (5,-7)],
                                [(10,2), (6,0), (2,5)],
                                [(0,0), (1,0), (2,9)] ]
        m3 = listToMatrix 4 1 [[(-1,0)],[(2,0)]]
    in
    putStr $ getMatrixStr $ tensorMatrices m1 m2
