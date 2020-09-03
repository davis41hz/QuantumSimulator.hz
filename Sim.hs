module Sim where
import           Complex
import           Matrix
import           Operations


-- State vector , which index in the vector we want probability of
observationProb :: Matrix -> Int -> Double
observationProb state observationIndex = amplitudeToProb (head (mData state!!observationIndex)) / squareNormVector state

-- Probability of transitioning from state1 to state2
transitionProb :: Matrix -> Matrix -> Complex
transitionProb state1 state2 =  divideComplex (innerProductVectors state2 state1) $ Complex (normVector state1 * normVector state2) 0

amplitudeVectorToProbVector :: Matrix -> Matrix
amplitudeVectorToProbVector m = Matrix (mRows m) (mCols m) $ map (\x -> [Complex (amplitudeToProb $ head x) 0]) (mData m)


expectationValueVariance :: Matrix -> Matrix -> Double -> Double
expectationValueVariance observable state mean = variance where
    varianceOperator = subtractMatrices observable $ scalarMultiplyMatrix (Complex mean 0) $ identityMatrix (mRows observable)
    variance = real $ innerProductVectors (multiplyMatrices (multiplyMatrices varianceOperator varianceOperator) state) state

expectationValue :: Matrix -> Matrix -> Double
expectationValue observable state
    | not $ isHermitian observable = error "Cannot calculate observable mean and variance when observable matrix isn't Hermitian"
    | otherwise = real $ innerProductVectors (multiplyMatrices observable state) state



measureState :: Double -> Matrix -> Int
measureState rng state = collapse rng 0 0 $ mData $ amplitudeVectorToProbVector state where
    collapse _ _ index [] = index - 1
    collapse target probSum index (p:px)
        | target > (probSum + real (head p)) = collapse target (probSum + real (head p)) (index + 1) px
        | otherwise = index


main = putStr $ getMatrixStr $ tensorSelfMatrix hadamardOp 2
