myDif :: Num a => [a] -> [a]
myDif [] = []
myDif [x] = []
myDif (x:xs:xm) = (xs-x) : myDif (xs:xm)

isAs :: (Eq a, Num a) => [a] -> Bool
isAs [] = True
isAs (x:xs) = allEqual(myDif (x:xs))