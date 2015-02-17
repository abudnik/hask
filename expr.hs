import Data.Char as Char

data Tree a = List a | Node a (Tree a) (Tree a)

parseValue :: String -> (String, String)
parseValue expr = break (not . Char.isDigit) expr

tokens :: String -> Maybe [String]
tokens [] = Just []
tokens expr | elem c operators = case followOp of
                                   Just f  -> Just ([c] : f)
                                   Nothing -> Nothing
            | Char.isDigit c   = case followVal of
                                   Just f -> Just (value : f)
                                   Nothing -> Nothing
            | Char.isSpace c   = followOp
            | otherwise        = Nothing
            where c = head expr
                  operators = ['+','-','*','/','(',')']
                  followOp = (tokens . tail) expr
                  (value, nextExpr) = parseValue expr
                  followVal = tokens nextExpr

main :: IO ()
main = do
  expr <- readFile "expression.txt"
  let exprTree = tokens expr
  putStrLn $ show exprTree
