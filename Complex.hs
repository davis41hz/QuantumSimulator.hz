module Complex where

data Complex = Complex {
    real      :: Double,
    imaginary :: Double
}

instance Show Complex where
    show (Complex r c) = show r ++ "+" ++ show c ++ "i"

instance Eq Complex where
    c1 == c2 = abs (real c1 - real c2) < 0.0000000000001 && abs (imaginary c1 - imaginary c2) < 0.00000000000001

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

amplitudeToProb :: Complex -> Double
amplitudeToProb (Complex r i) = r*r + i*i

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
