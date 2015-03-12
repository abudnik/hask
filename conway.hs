import Control.Monad
import Data.Array.IO
import System.Random
import Control.Concurrent

type Field = IOUArray (Int,Int) Int

fieldWidth = 80
fieldHeight = 40

initCell :: Field -> Int -> Int -> IO ()
initCell f i j = do
  v <- randomRIO (0,1)
  writeArray f (i,j) v

initField :: Field -> IO ()
initField f = do
  forM_ [1..fieldHeight] (\i ->
                           forM_ [1..fieldWidth] (\j -> initCell f i j))

showCell :: Int -> String
showCell v = if v == 0 then " " else "#"

printRow :: Field -> Int -> IO ()
printRow f i = do
  row <- forM [1..fieldWidth] (\j -> readArray f (i,j))
  putStrLn $ foldl (++) "" (map showCell row)

printField :: Field -> IO ()
printField f = forM_ [1..fieldHeight] (\i -> printRow f i)

countCells :: Field -> Int -> Int -> IO (Int)
countCells f i j = do
  arr <- mapM (readArray f) [(i + x, j + y) | x <- [-1..1], y <- [-1..1], not (x == 0 && y == 0)]
  return (foldl (+) 0 arr)

nextCellState :: Field -> Int -> Int -> IO (Int)
nextCellState f i j = do
  cur <- readArray f (i,j)
  cnt <- countCells f i j
  if cur == 1 && (cnt < 2 || cnt > 3) then do return(0)
    else if cur == 0 && cnt == 3 then do return(1)
         else do return(cur)

updateCell :: Field -> Field -> Int -> Int -> IO ()
updateCell f n i j = do
  s <- nextCellState f i j
  writeArray n (i,j) s

nextStep :: Field -> Field -> IO ()
nextStep f n = do
    forM_ [1..fieldHeight] (\i ->
                           forM_ [1..fieldWidth] (\j -> updateCell f n i j))

play :: Field -> Field -> IO ()
play f n = do
  nextStep f n
  printField n
  threadDelay $ 250 * 1000
  play n f

main :: IO ()
main = do
  field    <- newArray ((0, 0), (fieldHeight + 1, fieldWidth + 1)) 0 :: IO(Field)
  newField <- newArray ((0, 0), (fieldHeight + 1, fieldWidth + 1)) 0 :: IO(Field)
  initField field
  play field newField
