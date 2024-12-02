main = do
  contents <- readFile "input"
  let inp = lines contents
  print (length . filter getSafe . readLines $ inp)
  -- print inp

readInt :: String -> Int
readInt = read

readLines :: [String] -> [[Int]]
readLines [] = []
readLines xs = map (map readInt . words) xs

-- readLines :: [Int] -> [[Int]]
-- readLines [] = []
-- readLines xs = take 5 xs : readLines (drop 5 xs)

isSafe :: [Int] -> Bool -> Bool
isSafe [] _ = True
isSafe [_] _ = True
isSafe (x:y:xs) asc
    | asc && (x > y) = False
    | not asc && (x < y) = False
    | let dist = abs (x - y) in dist < 1 || dist > 3 = False
    | otherwise = isSafe (y:xs) asc

getSafe :: [Int] -> Bool
getSafe [] = True
getSafe [_] = True
getSafe (x:y:xs)
    | x > y = isSafe (x:y:xs) False
    | otherwise = isSafe (x:y:xs) True

