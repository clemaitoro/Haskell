circ :: Double -> Double
circ r = 2 * pi  *  r

area :: Double -> Double
area r = pi * r^2;

discr :: Double -> Double -> Double -> Double
discr a b c = b^2 - 4 * a * c

root1 :: Double -> Double -> Double -> Double
root1 a b c | d < 0 = error "discriminant negative"
            | otherwise = (-b + sqrt d) / (2 * a)
            where
                d = discr a b c

root2 :: Double -> Double -> Double -> Double
root2 a b c | d < 0 = error "discriminant negative"
            | otherwise = (-b - sqrt d) / (2 * a)
            where
                d = discr a b c

extrX :: Double -> Double -> Double -> Double
extrX a b c = -b / (2 * a)

extrY :: Double -> Double -> Double -> Double
extrY a b c = a * x^2 + b * x + c
              where
                  x = extrX a b c
