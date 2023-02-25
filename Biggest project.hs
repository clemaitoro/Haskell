

(~=) :: Double -> Double -> Bool
x ~= y = abs(x - y) <= epsilon -- replace undefined by your solution (for Question 2a)

-- For Question 1, you need to use this function. Nothing needs to be handed in
a = (1 - 1/3) ~= (2/3)     -- Change this as requested in the assignment (Question 2b)
b = (sqrt 2) ^ 2 == 2  -- Change this as requested in the assignment (Question 2b)

-

idx :: [Double] -> Profile
idx xs = zip [1..(length xs)] xs



values :: Profile -> [Double]
values [] = []
values xs = snd(head xs) : values(tail xs) where snd (a,b) = b


expand :: (Int, Int) -> Profile -> Profile
expand (n, m)    [] = zip [n..m] (repeat 0)
expand (n, m)  ((i,x):xs) = sort((zip [n..m] (repeat 0) ++ ((i,x):xs)) \\ [(z, 0.0) | z <- [n .. m], (i, x) <- (i, x):xs, i == z])


-

feasible :: Double -> Profile -> Bool
feasible c xs = sum (values xs) ~= c && all (>=0) (values xs)


test_feasible   = feasible 2.0 (idx [1,0,1])   -- Should give True
test_feasible'  = feasible 2.0 (idx [1,0,0])   -- Should give False
test_feasible'' = feasible 2.0 (idx [-1,0,3])  -- Should give False



costs :: Profile -> Profile -> Double
costs [] [] = 0
costs ((a,b):ps) ((c,d):xs) = (b+d)^2 + costs ps xs



-- This helps you to verify 'costs' for two examples, just evaluate it (it should give 'True'). Make sure that you understand the calculation below, this could help when implementing 'costs'
test_costs  = costs (idx [1,2,3]) (idx [3,2,1]) ~= (1+3)^2 + (2+2)^2 + (3+1)^2
test_costs' = costs (idx [4,0]) (idx [1,2]) ~= (4+1)^2 + (0+2)^2



-- The function 'f' from the pearl assignment description. Make sure you understand it, since 'g' is implemented similarly.
f :: Double -> Double -> Double -> Double -> Double -> Double
f x1 x2 c p1 p2 = (p1+x1)^2 + (p2+x2)^2 -- Don't change this, it is correct :-)


g :: Double -> Double -> Double -> Double -> Double
g x1 c p1 p2 = (p1+x1)^2 + (p2+c-x1)^2


prop_g x1 x2 p1 p2 = f x1 x2 c p1 p2 ~= g x1 c p1 p2
  where c = x1 + x2




-- Provide the derivative of 'g' here
g' :: Double -> Double -> Double -> Double -> Double
g' x1 c p1 p2 = 2*(x1+p1) - 2*(c-x1+p2)



opt2 :: Double -> Double -> Double -> (Double, Double)
opt2 c p1 p2 = (1/2*c + 1/2*p2 - 1/2*p1, 1/2*c + 1/2*p1 - 1/2*p2)

-- Checks if at the optimal x1 the derivative g' is zero
prop_opt2 c p1 p2 = g' x1 c p1 p2 ~= 0 && x1+x2 ~= c
  where (x1,x2) = opt2 c p1 p2

-- If we are at the optimal point, this means that moving away from it gives higher costs
prop_opt2' c p1 p2 =     f (x1+0.1) (x2-0.1) c p1 p2 >= f x1 x2 c p1 p2
                     &&  f (x1-0.1) (x2+0.1) c p1 p2 >= f x1 x2 c p1 p2
  where (x1,x2) = opt2 c p1 p2



proof :: String
proof = "Proof by mathematical induction"

charge :: Double -> Profile -> Profile
charge c ps = [(i, ((c + sum(values ps))/genericLength ps) - p) | (i,p) <- ps]




-- For N=2, 'charge' and 'opt2' should give the same answer. If it does not match, either one of the two functions has a problem. Also check prop_opt2 and prop_opt2'
prop_charge c p1 p2 = x1 ~= x1' && x2 ~= x2'
  where [x1, x2 ] = values $ charge c (idx [p1,p2])
        (x1',x2') = opt2 c p1 p2



numcandidates :: Int -> Int
numcandidates n = 2^n - 1



candidates :: Double -> Profile -> [Profile]
candidates c ps =  [charge c ps | feasible c (charge c ps)]


