-- bubble sort algorithm by Levin Rainey
-- levin.rainey.lrr@gmail.com
-- made public 14/11/23
-- v1

sort :: (Ord a) => [a] -> Int -> [a]
sort [] _ = []
sort list index
    -- check if sorted: yes = return list:
    | isSorted list = list
    -- check if index is at final list item: yes = start again with index 0
    | index == ((length list) - 1) = sort list 0
    -- otherwise = check if list !! index is greater than list !! (index + 1), if so then swap them and recurse with increased index by one, else recurse with increased index by one
    | (list !! index) > (list !! (index + 1)) = (sort (swapItems index (index + 1) list) (index + 1))
    | otherwise = sort list (index + 1)

isSorted :: (Ord a) => [a] -> Bool
isSorted [] = True
isSorted [_] = True
isSorted (x:y:xs) = (x <= y) && (isSorted (y:xs))

swapItems :: Int -> Int -> [a] -> [a]
swapItems i j list
    | i < 0 || j < 0 || i >= length list || j >= length list = list  -- Check if indices are within bounds
    | otherwise = map swapAtIndex $ zip [0..] list
    where
        swapAtIndex (index, element)
            | index == i = list !! j
            | index == j = list !! i
            | otherwise = element
