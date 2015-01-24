-- Problem 1: Find the last element of a list.
-- We decided that calling myLast on empty list throws error

myLast :: [a] -> a
myLast [a] = a
myLast (x:xs) = myLast xs

myLast' :: [a] -> a
myLast' xs = head (reverse xs)

-- Problem 2: Find the last but one element of a list.
-- We decided that calling myLast on empty list throws error

myButLast :: [a] -> a
myButLast  = myLast (init xs)

myButLast' :: [a] -> a
myButLast' (x:y:[]) = x
myButLast' (x:xs) = myButLast' xs

--Problem 3: Find the K'th element of a list. The first element in the list is number 1.

elementAt :: [a] -> Int -> a
elementAt xs a = xs !! (a-1)

-- Problem 4: Find the number of elements of a list.

myLength :: [a] -> Int
myLength = foldl (\acc _ -> acc + 1) 0

myLength' xs = sum [ 1 | _ <- xs ]
