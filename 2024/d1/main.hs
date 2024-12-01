main = do
  contents <- readFile "input"
  let inp = map readInt . words $ contents
  print (getSim . splitCols $ inp)

readInt :: String -> Int
readInt = read

splitCols :: [Int] -> ([Int], [Int])
splitCols [] = ([], [])
splitCols (x : y : xs) = (putSorted x l, putSorted y r)
  where
    (l, r) = splitCols xs

putSorted :: Int -> [Int] -> [Int]
putSorted x [] = [x]
putSorted y (x : xs)
  | y < x = y : x : xs
  | otherwise = x : putSorted y xs

getDist :: ([Int], [Int]) -> Int
getDist ([], []) = 0
getDist (x : xs, y : ys) = abs (x - y) + getDist (xs, ys)

getOcc :: Int -> [Int] -> Int
getOcc x [] = 0
getOcc y (x : xs)
  | x > y = 0
  | x == y = 1 + getOcc y xs
  | otherwise = getOcc y xs

-- part 2
getSim :: ([Int], [Int]) -> Int
getSim ([], _) = 0
getSim (x:xs, ys) = x * getOcc x ys + getSim (xs, ys)
