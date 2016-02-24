--Problem 1
lastElement :: [a] -> a
lastElement [a]    = a
lastElement (x:xs) = lastElement xs

--Problem 2
lastButOne :: [a] -> a
lastButOne (a:[b]) = a
lastButOne (x:xs)  = lastButOne xs

--Problem 3
elementAt :: [a] -> Int -> a
elementAt (x:xs) k
    | k > 1      = elementAt xs (k-1)
    | otherwise  = x

--Problem 4
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = myLengthH xs 1
    where 
        myLengthH :: [a] -> Int -> Int
        myLengthH [] k = k
        myLengthH (y:ys) k = myLengthH ys k+1

--Problem 5
myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = myReverse xs ++ [x]

--Problem 6
isPalindrome :: Eq a => [a] -> Bool 
isPalindrome [a] = True
isPalindrome []  = True
isPalindrome (x:xs)
    | x /= y    = False
    | otherwise = isPalindrome (myReverse ys)
        where (y:ys) = myReverse xs


--Problem 7
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List x) = concatMap flatten x

--Problem 8
compress :: Eq a => [a] -> [a]
compress [] = []
compress (x:xs) = compressH [x] xs
    where 
        compressH xs [] = xs
        compressH xs (y:ys)
            | last xs == y = compressH xs ys
            | otherwise    = compressH (xs++[y]) ys

--Problem 9
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = myReverse (packH [[x]] xs)

packH :: Eq a => [[a]] -> [a] -> [[a]]
packH xs [] = xs
packH (x:xs) (y:ys)
    | x' == y   = packH ((x':x':xs'):xs) ys
    | otherwise = packH ([y]:x:xs) ys
        where (x':xs') = x

--Problem 10
encode :: Eq a => [a] -> [(Int,a)] 
encode xs = zip lengths packs
    where
        lengths = [ l | l     <- (map length (pack xs))]
        packs   = [ x | (x:_) <-             (pack xs) ]

data Cardinality a = Multiple Int a | Single a
    deriving (Show)

--Problem 11
encodeModified :: [(Int,a)] -> [Cardinality a] 
encodeModified xs = map modify xs
    where
        modify (1,x) = Single x
        modify (l,x) = Multiple l x


--Problem 12
decodeModified :: [Cardinality a] -> [a]
decodeModified [] = []
decodeModified (x:xs) = decodeModifiedHelper x ++ decodeModified xs
    where
        decodeModifiedHelper (Multiple l x) = [ x | _<-[1..l]]
        decodeModifiedHelper (Single x)     = [ x ]


--Problem 14
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:(dupli xs)

--Problem 15
repli :: [a] -> Int -> [a]
repli [] _     = []
repli _  0     = []
repli (x:xs) n = (repli' x n) ++ (repli xs n)
    where 
        repli' :: a -> Int -> [a]
        repli' x 0 = []
        repli' x n = x:(repli' x (n-1))

--Problem 16
dropEvery [] _ = []
dropEvery xs n = myReverse(dropEveryHelper [] xs n n)
    where
        dropEveryHelper ys (x:xs) n 1 = dropEveryHelper ys xs n n 
        dropEveryHelper ys [] n m     = ys
        dropEveryHelper ys (x:xs) n m = dropEveryHelper (x:ys) xs n (m-1)


--Problem 17
split :: [a] -> Int -> ([a],[a])
split [] _     = ([],[])
split xs 0     = ([],xs)
split (x:xs) n = (x: ys, zs)
    where (ys,zs) = split xs (n-1)


--Problem 18
slice :: [a] -> Int -> Int -> [a]
slice xs i j = first (split (second (split xs (i-1))) (j-i+1))

first  (a,_) = a
second (_,b) = b

--Problem 19
rotate :: [a] -> Int -> [a]
rotate l 0 = l
rotate (x:xs) n = rotate (xs ++ [x]) (n-1)

--Problem 20
remove_at :: Int -> [a] -> ([a],[a])
