data Complex = Complex {
    real :: Double,
    imaginary :: Double
} deriving Show

addComplex :: Complex -> Complex -> Complex
addComplex (Complex r1 i1) (Complex r2 i2) = Complex (r1 + r2) (i1 + i2)

main =print (addComplex (Complex 1 2) (Complex 3 4))
