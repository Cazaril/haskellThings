import Data.Maybe
import Test.QuickCheck
import Control.Monad

myLast :: [a] -> Maybe a
myLast [] = Nothing
myLast (x:[]) = Just x 
myLast (x:xs) = myLast xs

myDrop :: [a] -> Int -> [a]
myDrop [] _ = []
myDrop x 0 = x
myDrop (x:xs) n = myDrop xs (n-1)

myTake :: [a] -> Int -> [a]
myTake [] _ = []
myTake x 0 = []
myTake (x:xs) 1 = [x]
myTake (x:xs) n = x:(myTake xs (n-1))

myHead :: [a] -> Maybe a
myHead [] = Nothing
myHead (x:_) = Just x

myTail :: [a] -> [a]
myTail [] = []
myTail (_:xs) = xs

myInit :: [a] -> [a]
myInit [] = []
myInit (x:[]) = []
myInit (x:y:[]) = [x]
myInit (x:y:xs) = [x]++(myInit (y:xs))

myButLast :: [a] -> Maybe a
myButLast [] = Nothing
myButLast (x:xs:[]) = Just x
myButLast (_:xs) = myButLast xs

elementAt :: [a] -> Int -> Maybe a
elementAt [] _ = Nothing
elementAt (x:_) 1 = Just x
elementAt (_:xs) b 
    | b < 1     = Nothing
    | otherwise = elementAt xs (b-1)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs++[x]

rElementAt :: [a] -> Int -> Maybe a
rElementAt (x:xs) b
    | b < 1 = elementAt (myReverse (x:xs)) (b*(-1))
    | b > 1 = elementAt (x:xs) b

myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs


isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome (x:xs) = (x:xs) == myReverse (x:xs)

data NestedList a = Elem a | List [NestedList a]
myFlatten :: NestedList a -> [a]
myFlatten (Elem a) = [a]
myFlatten (List []) = []
myFlatten (List (x:xs)) = (myFlatten x) ++ myFlatten (List xs)

myCompress :: (Eq a) => [a] -> [a]
myCompress [] = []
myCompress [x] = [x]
myCompress (x:y:xs) 
    | x == y = myCompress (x:xs)
    | otherwise = x:(myCompress (y:xs))

myPack :: (Eq a) => [a] -> [[a]]
myPack [] = []
myPack x = myPack' x [[]]

myPack' :: (Eq a) => [a] -> [[a]] -> [[a]]
myPack' [] z = z
myPack' (x:xs) [[]] = myPack' xs [[x]]
myPack' (x:xs) z
    | areEquals x (fromMaybe [] (myLast z)) = (myPack' xs (insertAtLast x z))
    | otherwise = (myPack' xs (z++[[x]]))

areEquals :: (Eq a) => a -> [a] -> Bool
areEquals _ [] = False
areEquals x (y:xs) =
    if x == y
        then True
        else False

insertAtLast :: (Eq a) => a -> [[a]] -> [[a]]
insertAtLast x [[]] = [[x]]
insertAtLast x z =
    if (fromMaybe [] (myLast z)) == []
        then [[x]]
        else (myInit z)++[((fromMaybe [] (myLast z))++[x])]

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = (f x):(myMap f xs)

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter f (x:xs)
    | f x = x:(myFilter f xs)
    | otherwise = myFilter f xs 

myEncode ::(Eq a) => [a] -> [(Int, a)]
myEncode [] = []
myEncode x = myEncode' (myPack x)

myEncode' :: [[a]] -> [(Int, a)]
myEncode' [] = []
myEncode' (x:xs) = (myLength x,fromJust (myHead x)):(myEncode' xs)

myZip :: (a -> b -> c) -> [a] -> [b] -> [c]
myZip _ [] _ = []
myZip _ _ [] = []
myZip f (x:xs) (y:ys) = (f x y):(myZip f xs ys)

myTailZip :: (a -> b -> c) -> [a] -> [b] -> [c]
myTailZip f (x:xs) (y:ys) = myTailZip' f (x:xs) (y:ys) []

myTailZip' :: (a -> b -> c) -> [a] -> [b] -> [c] -> [c]
myTailZip' _ [] _ z = z
myTailZip' _ _ [] z = z
myTailZip' f (x:xs) (y:ys) t = myTailZip' f xs ys t++[(f x y)]

mySanta :: String -> Int
mySanta xs = foldl (\acc x -> if x == ')' then acc + 1 else if x == '(' then acc-1 else acc+0) 0 xs

myIsPrime :: Int -> Bool
myIsPrime 0 = False
myIsPrime 1 = False
myIsPrime 2 = True
myIsPrime 3 = True
myIsPrime x = all (/= 0) (map (x `mod`) [2..(floor (sqrt (fromIntegral x)))])

hasOneZero :: [Int] -> Bool
hasOneZero [] = False
hasOneZero (0:xs) = hasOneZero' xs
hasOneZero (_:xs) = hasOneZero xs

hasOneZero' :: [Int] -> Bool
hasOneZero' [] = True
hasOneZero' (0:xs) = False
hasOneZero' (_:xs) = hasOneZero' xs

almostPrime :: Int -> Int -> Int
almostPrime a b = findPrimality [a..b]

listPrimes :: Int -> [Int]
listPrimes 0 = []
listPrimes n = myFilter myIsPrime [1..n]

isDivisor :: Int -> Int -> Bool
isDivisor x y = (x `mod` y == 0)

firstFactor :: Int -> [Int]
firstFactor n = myTake (myFilter (isDivisor n) (listPrimes n)) 1

factors :: Int -> [Int]
factors 1 = []
factors n = (firstFactor n)++(factors (n `div`(head (firstFactor n))))

findPrimality :: [Int] -> Int
findPrimality [] = 0
findPrimality (x:xs) = 
    if (myLength (factors x) == 2)
        then 1+(findPrimality xs)
        else findPrimality xs

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) =
    smallSort ++ [x] ++ bigSort
    where smallSort = quickSort (filter (<= x) xs) 
    where bigSort = quickSort (filter (>x) xs) 