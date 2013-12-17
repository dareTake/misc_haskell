-- Ninety - Nine Haskell Programs
-- Q 1 to 10
--
import Data.List( group )

--Q1
last' :: [a] -> a
last' = foldr1 (flip  const)

--Q2
myButLast' :: [a] -> a
myButLast' = last . init 

--Q3
findK :: Int -> [a] -> a
findK n xs = xs !! (n - 1)

findK' _ []     = error "Too large index"
findK' 1 (x:xs) = x
findK' n (x:xs) | n < 1     = error "Invalid index" 
                | otherwise = findK' (n-1) xs

--Q4
length' :: [a] -> Int 
length' = foldr (const (+1))  0 

--Q5
reverse' :: [a] -> [a] 
reverse' = foldl (\acc x -> x: acc) []
reverse'' = foldl (flip (:)) []

--Q6
palin :: (Eq a) => [a] -> Bool
palin   xs = reverse xs == xs
palin'  xs = foldr (\(x,y) acc -> acc && x == y) True (zip xs (reverse xs))
palin'' xs = and $ zipWith (==) xs (reverse xs)

--Q8
compress :: (Eq a) => [a] -> [a]
compress = foldr rem []  
          where
              rem x []  = [x]
              rem x acc | x == head acc = acc
                        | otherwise     = x : acc

compress' :: (Eq a) => [a] -> [a] 
compress' xs = map head $ group xs

--Q9
pack :: (Eq a) => [a] -> [[a]]
pack ys =  getDuplicates []  ys
        where
            spanUnequal acc  []    = acc
            spanUnequal []  (x:xs) = getDuplicates [] (x:xs)
            spanUnequal acc (x:xs) | x == head (head acc) = spanUnequal acc xs
                                   | otherwise            = getDuplicates acc (x:xs) 
            getDuplicates acc (x:xs) = if null dup 
                                          then spanUnequal acc  xs  
                                          else spanUnequal (dup : acc) xs
                                          where 
                                                dup = takeWhile (== x) (x:xs)

pack' :: (Eq a) => [a] -> [[a]]
pack' [x] = [[x]] 
pack' (x:xs) = takeWhile (== x) (x:xs) : pack' ( dropWhile (== x) xs)


--Q10
--
runLength :: (Eq a) => [a] -> [(Int, a)]
runLength xs = zip (map length packData) (map head packData )
             where
                 packData = pack' xs

--Q11
data ListItem a = Single a 
                | Multiple Int a
                deriving (Show)

encodeModified :: (Eq a) => [a] -> [ListItem a] 
encodeModified  = map encode . runLength 
                where encode  (1,y) = Single y 
                      encode  (x,y) = Multiple x y


--Q12
decodeModified :: [ListItem a] -> [a]
decodeModified = concat . map decode  --same as concatMap
               where
                   decode (Single x)     = [x]
                   decode (Multiple x y) = replicate x y 

--Q13
encodeDirect :: (Eq a) => [a] -> [ListItem a]
encodeDirect = map encodeListItem . foldr tupleList []  
            where
                tupleList x [] = [(1,x)]
                tupleList x acc@((i,e):xs) | e == x    = (1+ i, e): xs 
                                           | otherwise = (1,x)    : acc
                encodeListItem  (1,y) = Single y 
                encodeListItem  (x,y) = Multiple x y

                                
--Q14
dupli :: [a] -> [a] 
dupli = concatMap (replicate 2)

--Q15
repli :: [a] -> Int -> [a]
repli xs n = concatMap (replicate n) xs
repli' = flip $ concatMap . replicate

--Q16
dropEvery :: (Eq a) => [a] -> Int -> [a]
dropEvery [] n = []
dropEvery (x:xs) n | length xs `mod` n == 0 = x : dropEvery xs n
                   | otherwise = dropEvery xs n

dropEvery' xs n = map fst $ filter ((n/=) . snd) $ zip xs (cycle [1..n])

--Q17
split :: [a] -> Int -> [[a]]
split xs n = map (map fst) (filter ( (<= n) . snd) tupleList : [ filter ( (> n) . snd) tupleList])
        where
            tupleList = zip xs [1..]

--Q18
slice :: [a] -> Int -> Int -> [a]
slice xs p q =  fst .  unzip $ filter ((>= p) . snd)  tupleList
            where 
                  tupleList = zip xs [1..q]

--Q20
remove_at :: [a] -> Int -> [a] 
remove_at xs n = map fst $ filter ( (/= n) . snd) (zip xs [1..])

--Q21
insertAt :: a -> [a] -> Int -> [a]
insertAt val xs index = concat $  head splitTuple : (val : (head . tail) splitTuple) : [] 
                    where 
                          splitTuple = split xs index

insertAt' e xs 1 = e : xs
insertAt' e (x:xs) n = x: insertAt' e xs (n - 1)

--Q22
range :: Int -> Int -> [Int]
range p q  | p == q    = [p]
           | otherwise = p : range (p + 1) q 
                    

--Q23 to Q30 , need more info on IO, Random & Stuff
--

--Q31
isPrime :: Integer -> Bool
isPrime n | n == 2    = True
          | otherwise = not $  any (\x -> n `mod` x == 0)  [2..maxN]
        where
            maxN = ceiling $ sqrt $ fromIntegral n 

--Q32
gcd' :: Int ->  Int -> Int
gcd' p q | p < 0 = gcd' (-p)   q
         | q < 0 = gcd'   p  (-q)
         | q == 0 = p
         | otherwise = gcd' q  (p `mod` q)

--Q33
coprime :: Int -> Int -> Bool
coprime p q | gcd' p q == 1 = True
coprime _ _ = False

--Q34
totient :: Int -> Int
totient n = length $ filter (coprime n) [1..n-1] 

