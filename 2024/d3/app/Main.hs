import Text.Regex.TDFA

main :: IO ()
main = do
  contents <- readFile "input"
  print (parseMul2 (parseInp contents) True) -- sum . map parseMul $ 

readInt :: String -> Int
readInt = read

parseInp :: String -> [String]
parseInp str = getAllTextMatches (str =~ "do\\(\\)|don't\\(\\)|mul\\([0-9]{1,4},[0-9]{1,4}\\)")

parseMul :: String -> Int
parseMul str = let x:y:_ = getAllTextMatches( str =~ "[0-9]+" ) in readInt x * readInt y

parseMul2 :: [String] -> Bool -> Int
parseMul2 [] _ = 0
parseMul2 (x:xs) todo
    | x == "do()" = parseMul2 xs True
    | x == "don't()" = parseMul2 xs False
    | todo = parseMul x + parseMul2 xs todo
    | otherwise = parseMul2 xs todo

