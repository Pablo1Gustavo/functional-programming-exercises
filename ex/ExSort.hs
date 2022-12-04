module Sort (sort) where

sort :: Ord a => [a] -> [a]
sort = qsort

halve :: [a] -> ([a],[a])
halve [] = ([],[])
halve [x] = ([x],[])
halve (l:r:xs) = (l:ls, r:rs)
    where
        (ls,rs) = halve xs

-- Require two sorted lists
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x <= y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort fs) (msort ss)
    where
        (fs, ss) = halve xs

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = smaller ++ [x] ++ larger
    where
        smaller = qsort (filter (<= x) xs)
        larger = qsort (filter (> x) xs)

-- Require a sorted list
insert :: Ord a => a -> [a] -> [a]
insert v [] = [v]
insert v (x:xs)
    | v <= x = v:x:xs
    | otherwise = x : insert v xs

isort :: Ord a => [a] -> [a]
isort = foldr insert []