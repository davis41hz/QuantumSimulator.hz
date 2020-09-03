import           Matrix
import           Operations
import           Sim
import           System.Random

deutschIsConstant :: Double -> Matrix -> Bool
deutschIsConstant rng blackbox = endState == 0 || endState == 1 where
    startState = Matrix 4 1 [[zero],[one], [zero], [zero]]
    circuit = serialOperations [tensorMatrices hadamardOp $ identityOp 2, blackbox, tensorMatrices hadamardOp hadamardOp]
    endState = measureState rng $ multiplyMatrices circuit startState

-- The folowing matrices represent the unitary operator that takes |x,y> to |x, y XOR f(x)>
constant0 :: Matrix -- f(1) = f(0) = 0
constant0 = identityOp 4

constant1 :: Matrix -- f(1) = f(0) = 1
constant1 = Matrix 4 4 [[zero, one, zero, zero],
                        [one, zero, zero, zero],
                        [zero, zero, zero, one],
                        [zero, zero, one, zero]]

balancedIdentity :: Matrix -- f(0) = 0; f(1) = 1
balancedIdentity = cnotOp

balancedNot :: Matrix -- f(0) = 1; f(1) = 0
balancedNot = Matrix 4 4 [  [zero, one, zero, zero],
                            [one, zero, zero, zero],
                            [zero, zero, one, zero],
                            [zero, zero, zero, one] ]


-- main :: IO ()
-- main = do
--     rng <- randomIO :: IO Double
--     let chosenFunction = balancedIdentity
--     if deutschIsConstant rng chosenFunction
--     then putStrLn "The blackbox is constant"
--     else putStrLn "The blackbox is balanced"
