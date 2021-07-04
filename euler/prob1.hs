multiples :: Int -> [Int] -> [Int]
multiples upto divs = filter (isdiv divs) [1..(upto-1)]

isdiv :: [Int] -> Int -> Bool
isdiv (d:ds) x
    | mod x d == 0  = True
    | null ds       = False
    | otherwise     = isdiv ds x

main = print (sum $ multiples 1000 [3,5])