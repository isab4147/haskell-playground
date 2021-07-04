nextFibbo :: Int -> Int -> Int
nextFibbo a b = a + b

fibbo :: Int -> [Int]
fibbo upto = genFibbo 1 2 [2,1]
  where
    genFibbo a b xs
      | c > upto  = xs
      | otherwise = genFibbo b c (c:xs)
      where
        c = a + b

main = print (sum $ filter even $ fibbo 4000000)