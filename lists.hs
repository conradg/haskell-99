lastElement :: [a] -> a
lastElement [a]    = a
lastElement (x:xs) = lastElement xs

lastButOne :: [a] -> a
lastButOne (a:[b]) = a
lastButOne (x:xs)  = lastButOne xs

elementAt :: [a] -> Int -> a
elementAt (x:xs) k
    | k > 1      = elementAt xs (k-1)
    | otherwise  = x

myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = myLengthH xs 1
    where 
        myLengthH :: [a] -> Int -> Int
        myLengthH [] k = k
        myLengthH (y:ys) k = myLengthH ys k+1

myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = myReverse xs ++ [x]


isPalindrome :: Eq a => [a] -> Bool 
isPalindrome [a] = True
isPalindrome []  = True
isPalindrome (x:xs)
    | x /= y    = False
    | otherwise = isPalindrome (myReverse ys)
        where (y:ys) = myReverse xs

data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List x) = concatMap flatten x

pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = myReverse (packH [[x]] xs)

packH :: Eq a => [[a]] -> [a] -> [[a]]
packH xs [] = xs
packH (x:xs) (y:ys)
    | x' == y   = packH ((x':x':xs'):xs) ys
    | otherwise = packH ([y]:x:xs) ys
        where (x':xs') = x

encode :: Eq a => [a] -> [(Int,a)] 
encode xs = zip lengths packs
    where
        lengths = [ l | l     <- (map length (pack xs))]
        packs   = [ x | (x:_) <-             (pack xs) ]

data Cardinality a = Multiple Int a | Single a
    deriving (Show)

encodeModified :: [(Int,a)] -> [Cardinality a] 
encodeModified xs = map modify xs
    where
        modify (1,x) = Single x
        modify (l,x) = Multiple l x


decodeModified :: [Cardinality a] -> [a]
decodeModified [] = []
decodeModified (x:xs) = decodeModifiedHelper x ++ decodeModified xs
    where
        decodeModifiedHelper (Multiple l x) = [ x | _<-[1..l]]
        decodeModifiedHelper (Single x)     = [ x ]

dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:(dupli xs)

repli :: [a] -> Int -> [a]
repli [] _     = []
repli _  0     = []
repli (x:xs) n = (repli' x n) ++ (repli xs n)
    where 
        repli' :: a -> Int -> [a]
        repli' x 0 = []
        repli' x n = x:(repli' x (n-1))

dropEvery [] _ = []
dropEvery xs n = myReverse(dropEveryHelper [] xs n n)
    where
        dropEveryHelper ys (x:xs) n 1 = dropEveryHelper ys xs n n 
        dropEveryHelper ys [] n m     = ys
        dropEveryHelper ys (x:xs) n m = dropEveryHelper (x:ys) xs n (m-1)


split :: [a] -> Int -> ([a],[a])
split (x:xs) n = splitHelper [] (x:xs) n
    where 
        splitHelper ys xs 0     = (ys,xs)
        splitHelper ys (x:xs) n = splitHelper (ys ++ [x]) xs n-1
