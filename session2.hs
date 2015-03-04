-- 5 myReverse

myReverse :: [a] -> [a]
myReverse = foldl (\acc x -> x:acc) []



-- 6 isPalindrome

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == reverse xs

data NestedList a = Elem a | List [NestedList a] deriving (Show)
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

-- 8 compress

compress :: (Eq a) => [a] -> [a]
compress [] = []
compress (x:xs) = x : (compress $ dropWhile (== x) xs)

-- 9 pack

pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack xs@(x:_) = p : (pack rest)
    where (p, rest) = span (== x) xs

pack' [] = []
pack' xs@(x:_) = takeWhile (==x) xs : pack (dropWhile (==x) xs)
