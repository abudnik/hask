import Data.Char as Char

data Tree a = Leaf a | Node a (Tree a) (Tree a) | Empty
    deriving (Show)

data TokenType = Operator | Value | Unknow
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
tokens expr | elem c operators = case followOp of
                                   Just f  -> Just (newOperator [c] : f)
                                   Nothing -> Nothing
            | Char.isDigit c   = case followVal of
                                   Just f -> Just (newValue value : f)
                                   Nothing -> Nothing
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

parseVal :: Token -> [Token] -> Maybe TokTree
parseVal t [] = Just (Leaf t)
parseVal t (x:xs) = case nextTokType of
                      Value -> Nothing
                      Operator -> case follow of
                                    Just right -> Just (Node nextTok (Leaf x) right)
                                    Nothing -> Nothing
    where nextTok = head xs
          nextTokType = tokenTyp nextTok
          follow = parse xs


parse :: [Token] -> Maybe TokTree
parse [] = Just Empty
parse (x:xs) | tokenTyp x == Operator = case value of
                                          "(" -> Just Empty
                                          ")" -> Just Empty
             | tokenTyp x == Value = parseVal x xs
             where value = tokenVal x

main :: IO ()
main = do
  expr <- readFile "expression.txt"
  let toks = tokens expr
  let maybeTree = case toks of
                    Just ts -> parse ts
                    Nothing -> parse []
  let tree = case maybeTree of
               Just t -> t
               Nothing -> Empty
  putStrLn $ printBinTree tree 0
