
import Data.Char as Char

splitStr :: (Char -> Bool) -> String -> [String]
splitStr f "" = []
splitStr f s = let (word, part) = break f (dropWhile f s)
               in word : splitStr f (dropWhile f part)

checkDivisors :: Int -> Int -> Int -> Bool
checkDivisors v d maxd | d <= maxd = if mod v d == 0 then False else checkDivisors v (d + 1) maxd
                       | otherwise = True

isPrime :: Int -> Bool
isPrime v | v > 2  = if odd v then checkDivisors v 3 ((floor . sqrt . fromIntegral) v) else False
          | otherwise = False

findPrimes :: Int -> Int -> [Int]
findPrimes fst lst = filter isPrime [fst..lst]

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let words = splitStr Char.isSpace contents
  let first = read (head words) :: Int
  let last = read ((head . tail) words) :: Int
  mapM_ (putStr . (++ ",") . show) (findPrimes first last)

