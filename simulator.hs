-- COMPLEX ARITHMETIC
data Complex = Complex {
    real      :: Double,
    imaginary :: Double
}

instance Show Complex where
    show (Complex r c) = show r ++ "+" ++ show c ++ "i"

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


scalarMultiplyMatrix :: Complex -> Matrix -> Matrix
scalarMultiplyMatrix scalar m = Matrix (mRows m) (mCols m) $ map (map (multiplyComplex scalar)) (mData m)

inverseMatrix :: Matrix -> Matrix
inverseMatrix m = scalarMultiplyMatrix (Complex (-1) 0) m

multiplyMatrices :: Matrix -> Matrix -> Matrix
multiplyMatrices m1 m2
    | (mCols m1) /= (mRows m2) = error $ "Matrix multiplication incompatible with matrices " ++ (show $ mRows m1) ++ "x" ++ (show $ mCols m1) ++ " and "++ (show $ mRows m2) ++ "x" ++ (show $ mCols m2)
    | otherwise = Matrix (mRows m1) (mCols m2) $ map ((\x -> foldl1 (zipWith addComplex) x) . zipWith (flip (map . multiplyComplex)) (mData m2)) (mData m1)

listToMatrix :: Int -> Int -> [[(Double,Double)]] -> Matrix
listToMatrix r c l = Matrix r c $ map (map (\x -> Complex (fst x) (snd x))) l

main :: IO ()
main =
    let m1 = listToMatrix 4 4 [ [(1,0),(0,0),(0,0),(0,0)],
                                [(0,0),(1,0),(0,0),(0,0)],
                                [(0,0),(0,0),(0,0),(1,0)],
                                [(0,0),(0,0),(1,0),(0,0)]]
        m2 = listToMatrix 4 1 [[(0,0)],[(0,0)],[(0,0)],[(1,0)]]
    in
    putStr $ getMatrixStr $ multiplyMatrices m1 m2
