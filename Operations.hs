module Operations where
import           Complex
import           Matrix

serialOperations :: [Matrix] -> Matrix
serialOperations = foldl1 multiplyMatrices

parallelOperations :: [Matrix] -> Matrix
parallelOperations = foldl1 tensorMatrices

identityOp :: Int -> Matrix
identityOp = identityMatrix

zero :: Complex
zero = Complex 0 0

one :: Complex
one = Complex 1 0

hFactor :: Complex
hFactor = Complex (1 / sqrt 2) 0

half :: Complex
half = Complex 0.5 0.5

halfConj :: Complex
halfConj = Complex 0.5 (-0.5)

hadamardOp :: Matrix
hadamardOp = Matrix 2 2 [   [hFactor, hFactor],
                            [hFactor, multiplyComplex (Complex (-1) 0) hFactor]]

pauliXOp :: Matrix
pauliXOp = Matrix 2 2 [ [zero, one],
                        [one, zero]]

notOp :: Matrix
notOp =  pauliXOp

pauliYOp :: Matrix
pauliYOp = Matrix 2 2 [ [zero, Complex 0 (-1)],
                        [Complex 0 1, zero]]

pauliZOp :: Matrix
pauliZOp = Matrix 2 2 [ [Complex 1 0, zero],
                        [zero, Complex (-1) 0]]

cnotOp :: Matrix
cnotOp = Matrix 4 4 [   [one, zero, zero, zero],
                        [zero, one, zero, zero],
                        [zero, zero, zero, one],
                        [zero, zero, one, zero] ]

toffoliOp :: Matrix
toffoliOp = Matrix 8 8 [[one, zero, zero, zero, zero, zero, zero, zero],
                        [zero, one, zero, zero, zero, zero, zero, zero],
                        [zero, zero, one, zero, zero, zero, zero, zero],
                        [zero, zero, zero, one, zero, zero, zero, zero],
                        [zero, zero, zero, zero, one, zero, zero, zero],
                        [zero, zero, zero, zero, zero, one, zero, zero],
                        [zero, zero, zero, zero, zero, zero, zero, one],
                        [zero, zero, zero, zero, zero, zero, one, zero]]

fredkinOp :: Matrix
fredkinOp = Matrix 8 8 [[one, zero, zero, zero, zero, zero, zero, zero],
                        [zero, one, zero, zero, zero, zero, zero, zero],
                        [zero, zero, one, zero, zero, zero, zero, zero],
                        [zero, zero, zero, one, zero, zero, zero, zero],
                        [zero, zero, zero, zero, one, zero, zero, zero],
                        [zero, zero, zero, zero, zero, zero, one, zero],
                        [zero, zero, zero, zero, zero, one, zero, zero],
                        [zero, zero, zero, zero, zero, zero, zero, one]]


swapOp :: Matrix
swapOp = Matrix 4 4 [   [one, zero, zero, zero],
                        [zero, zero, one, zero],
                        [zero, one, zero, zero],
                        [zero, zero, zero, one] ]

rootSwapOp :: Matrix
rootSwapOp = Matrix 4 4 [   [one, zero, zero, zero],
                            [zero, half, halfConj, zero],
                            [zero, halfConj, half, zero],
                            [zero, zero, zero, one] ]

rootNotOp :: Matrix
rootNotOp = Matrix 2 2 [[half, halfConj],
                        [halfConj, half]]


phaseShiftOp :: Double -> Matrix
phaseShiftOp theta = Matrix 2 2 [[one, zero],
                                [zero, Complex (cos theta) (sin theta)]]

sOp :: Matrix
sOp = Matrix 2 2 [  [one, zero],
                    [zero, Complex 0 (-1)]]

tOp :: Matrix
tOp = phaseShiftOp (pi/4)

cU :: Matrix -> Matrix
cU u = Matrix (2 * mRows u) (2 * mRows u) $ concatV (concatH (identityMatrix $ mRows u) (zeroMatrix $ mRows u)) (concatH (zeroMatrix $ mRows u) u) where
    concatH m1 m2 = zipWith (++) (mData m1) (mData m2)
    concatV d1 d2 = d1 ++ d2

deutschOp :: Double -> Matrix
deutschOp theta = Matrix 8 8 [  [one, zero, zero, zero, zero, zero, zero, zero],
                                [zero, one, zero, zero, zero, zero, zero, zero],
                                [zero, zero, one, zero, zero, zero, zero, zero],
                                [zero, zero, zero, one, zero, zero, zero, zero],
                                [zero, zero, zero, zero, one, zero, zero, zero],
                                [zero, zero, zero, zero, zero, one, zero, zero],
                                [zero, zero, zero, zero, zero, zero, Complex 0 (cos theta), Complex (sin theta) 0],
                                [zero, zero, zero, zero, zero, zero, Complex (sin theta) 0,  Complex 0 (cos theta)]]
