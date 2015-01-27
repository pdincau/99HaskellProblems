-- 5 myReverse

myReverse :: [a] -> [a]
myReverse = foldl (\acc x -> x:acc) []



-- 6 isPalindrome

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == reverse xs


-- 8 compress

compress :: (Eq a) => [a] -> [a]
compress [] = []
compress (x:xs) = x : (compress (dropWhile equal xs))
  where equal y = y == x

-- 9 pack

pack :: (Eq a) => [a] -> [[a]]
