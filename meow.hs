-- class 1

elemi :: (Eq a) => a -> [a] -> Bool
elemi a (x:xs)
  | x == a   = True
  | null xs  = False
  | otherwise = elemi a xs


nub :: (Eq a) => [a] -> [a]
nub (x:xs)
  | null xs     = [x]
  | elemi x xs  = nub xs
  | otherwise   = x:nub xs


isAsc :: [Int] -> Bool 
isAsc (x:xs) = length xs <= 1 || ((x <= head xs) && isAsc xs)


hasPath :: [(Int,Int)] -> Int -> Int -> Bool
hasPath xs a b =
  let x = fst (head xs)
      y = snd (head xs)
  in
    if a == x then
      b == y || hasPath xs y b
    else
      (length xs /= 1) && hasPath (tail xs) a b


-- class 2

rev :: [a] -> [a]
rev = foldl (flip (:)) []


prefixes :: [a] -> [[a]]
--prefixes xs = rev $ foldr ((:).rev) [] $ foldl (\acc x -> (x : (if null acc then [] else head acc)) : acc) [] xs
-- or
prefixes = foldr (\x acc -> [x] : map (x :) acc) []


lagrange :: [(Float,Float)] -> Float -> Float
lagrange ts x =
  foldr (\(a,b) acc
    -> acc + b *
      foldr (\x' acc' -> acc' * (x - x')/(a - x')) 1 [ a' | (a',_) <- ts, a /= a' ] ) 0 ts
      

data Trie a = Leaf a | Node a [Trie a]

foldtrie :: (b -> a -> b) -> b -> Trie a -> b
foldtrie f acc  (Leaf x) = f acc x
foldtrie f acc (Node x xs) = foldl f' (f acc x) xs
  where
    f' acc t = foldtrie f acc t

-- test = Node 'c' [Node 'a' [Leaf 'r', Leaf 't'], Node 'o' [Node 'o' [Leaf 'l']]]

-- class 3

main :: IO ()
main = print ()
