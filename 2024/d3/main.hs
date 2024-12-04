main = do
  contents <- readFile "input"
  let inp = lines contents
  print (matchRegex (mkRegex "mul\\(\\d{1,4},\\d{1,4}\\)") inp)
  -- print (isSafeDamp [3,1,2,4,5] False False)

readInt :: String -> Int
readInt = read

readLines :: [String] -> [[Int]]
readLines [] = []
readLines xs = map (map readInt . words) xs

isSafe :: [Int] -> Bool -> Bool
isSafe [] _ = True
isSafe [_] _ = True
isSafe (x:y:xs) asc
    | wrong = False
    | otherwise = isSafe (y:xs) asc
    where wrong = (let dist = abs (x - y) in dist < 1 || dist > 3) || ((asc && (x > y)) || (not asc && (x < y)))

getSafe :: [Int] -> Bool
getSafe [] = True
getSafe [_] = True
getSafe (x:y:xs)
    | x > y = isSafe (x:y:xs) False
    | otherwise = isSafe (x:y:xs) True

-- part 2

gggg :: [Int] -> Bool
gggg xs = getSafeDamp xs (-1)

getSafeDamp :: [Int] -> Int -> Bool
getSafeDamp [] _ = True
getSafeDamp [_] _ = True
getSafeDamp xs i
    | i >= length xs = False
    | i < 0 = isSafeDamp xs
    | otherwise = isSafeDamp (deleteAt i xs)
    where
        x:y:_
            | i < 0 = xs
            | otherwise = deleteAt i xs
        asc = x < y
        isSafeDamp [] = True
        isSafeDamp [_] = True
        isSafeDamp (h:m:hs)
                | wrong = getSafeDamp xs (i+1)
                | otherwise = isSafeDamp (m:hs)
                where wrong = (let dist = abs (h-m) in dist < 1 || dist > 3) || ((asc && (h > m)) || (not asc && (h < m)))

deleteAt :: Int -> [a] -> [a]
deleteAt _ [] = []
deleteAt _ [x] = [x]
deleteAt idx xs = l ++ r
  where l = take idx xs
        (_:r) = drop idx xs
