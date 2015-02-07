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

-- or
--
-- compress (x:xs) = x : (compress $ dropWhile equal xs)
--   where equal y = y == x



-- 9 pack
-- Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.

pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack xs@(x:_) = p : (pack rest)
    where (p, rest) = span (== x) xs
