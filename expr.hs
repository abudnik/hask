import Data.Char as Char

data Tree a = Leaf a | Node a (Tree a) (Tree a) | Empty
    deriving (Show)

data TokenType = Operator | Value
    deriving (Show, Eq)

data Token = Token {
      ttype :: TokenType,
      val   :: String
    }
    deriving (Show)

type TokTree = Tree Token

tokenTyp :: Token -> TokenType
tokenTyp (Token ttype _) = ttype
    
tokenVal :: Token -> String
tokenVal (Token _ value) = value

newOperator :: String -> Token
newOperator v = Token {ttype = Operator, val = v }

newValue :: String -> Token
newValue v = Token {ttype = Value, val = v }

parseValue :: String -> (String, String)
parseValue expr = break (not . Char.isDigit) expr

tokens :: String -> Maybe [Token]
tokens [] = Just []
tokens expr | elem c operators = followOp >>= (\f -> return (newOperator [c] : f))
            | Char.isDigit c   = followVal >>= (\f -> return (newValue value : f))
            | Char.isSpace c   = followOp
            | otherwise        = Nothing
            where c = head expr
                  operators = ['+','-','*','/','(',')']
                  followOp = (tokens . tail) expr
                  (value, nextExpr) = parseValue expr
                  followVal = tokens nextExpr

printNode :: Show a => a -> Int -> String
printNode s level = replicate level '-' ++ show s ++ "\n"
printBinTree :: Show a => Tree a -> Int -> String
printBinTree Empty _  = []
printBinTree (Leaf v) level = printNode v level
printBinTree (Node v left right) level = printNode v level ++ printBinTree left (level + 1) ++ printBinTree right (level + 1)


findOperator :: [Token] -> [String] -> Int -> Int -> (Bool, Int)
findOperator [] _ _ _ = (False, 0)
findOperator (x:xs) ops c pos | tokenVal x == "(" = findOperator xs ops (c + 1) (pos + 1)
                              | tokenVal x == ")" = findOperator xs ops (c - 1) (pos + 1)
                              | tokenIsOperation = (True, pos)
                              | otherwise = findOperator xs ops c (pos + 1)
                              where tokenIsOperation = c == 0 && tokenTyp x == Operator &&
                                                       elem (tokenVal x) ops

findOperatorPrior :: [Token] -> (Bool, Int)
findOperatorPrior toks | foundPlusMinus = (True, pos)
                       | otherwise = findOperator toks ["*", "/"] 0 0
                       where (foundPlusMinus, pos) = findOperator toks ["+", "-"] 0 0

parseTok :: [Token] -> Maybe TokTree
parseTok [] = Nothing
parseTok (x:xs) | isValue && null xs = Just (Leaf x)
                | otherwise = Nothing
                where isValue = tokenTyp x == Value
                                    
parse :: [Token] -> Maybe TokTree
parse [] = Just Empty
parse (x:xs) | tokenVal x == "(" && (tokenVal $ last xs) == ")" = parse $ init xs
             | foundOperator = leftTree >>= (\l ->
                                             rightTree >>= (\r ->
                                                            return (Node ((x:xs) !! opPos) l r)))
             | otherwise = parseTok (x:xs)
             where (foundOperator, opPos) = findOperatorPrior (x:xs)
                   (left, right) = splitAt opPos (x:xs)
                   leftTree = parse left
                   rightTree = (parse . tail) right

eval :: TokTree -> Int {-- Maybe Int--}
eval Empty = 0
eval (Leaf v) = (read . tokenVal) v
eval (Node n l r) = case tokenVal n of
                      "+" -> (eval l) + (eval r)
                      "-" -> (eval l) - (eval r)
                      "*" -> (eval l) * (eval r)
                      "/" -> (eval l) `div` (eval r)

main :: IO ()
main = do
  expr <- readFile "expression.txt"
  let toks = tokens expr
  let maybeTree = toks >>= (\ts -> parse ts)
  let tree = case maybeTree of
               Just t -> t
               Nothing -> Empty
  putStrLn $ printBinTree tree 0
  putStrLn $ (show . eval) tree
