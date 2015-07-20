import Data.Char (ord, chr)
import Control.Monad (void)
import Data.Word (Word8)
import Data.Maybe (mapMaybe)

data Zip a = Zip [a] a [a] deriving (Show)

data BFToken = Inc 
             | Dec 
             | PointLeft 
             | PointRight 
             | Block [BFToken]
             | BlockOpen
             | BlockClose
             | Print 
             | Read
             | Comment Char
             | Debug 
             deriving (Show)

type Data   = Zip Word8

emptyData :: Data
emptyData = Zip (repeat 0) 0 (repeat 0)

buildAST :: String -> [BFToken]
buildAST = fst . buildAST' . mapMaybe parseChar

buildAST' :: [BFToken] -> ([BFToken], [BFToken])
buildAST' [] = ([], [])
buildAST' (BlockOpen : xs) = (Block b : b', xs'')
  where (b, xs') = buildAST' xs
        (b', xs'') = buildAST' xs'
buildAST' (BlockClose : xs) = ([], xs)
buildAST' (x : xs) = (x : is, xs')
  where (is, xs') = buildAST' xs

mapPtr :: (a -> a) -> Zip a -> Zip a
mapPtr f (Zip l x r)
  = Zip l (f x) r

getPtr :: Zip a -> a
getPtr (Zip _ p _) = p

putPtr :: Zip a -> a -> Zip a
putPtr (Zip l _ r) x = Zip l x r

moveLeft :: Zip a -> Zip a
moveLeft (Zip [] _ _) = undefined
moveLeft (Zip (l : ls) p rs)
  = Zip ls l (p : rs)

moveRight :: Zip a -> Zip a
moveRight (Zip _ _ []) = undefined
moveRight (Zip ls p (r : rs))
  = Zip (p : ls) r rs

parseChar :: Char -> Maybe BFToken
parseChar '+' = Just Inc
parseChar '-' = Just Dec
parseChar '<' = Just PointLeft
parseChar '>' = Just PointRight
parseChar '.' = Just Print
parseChar ',' = Just Read
parseChar '[' = Just BlockOpen
parseChar ']' = Just BlockClose
parseChar '#' = Just Debug
parseChar _   = Nothing

run :: [BFToken] -> Data -> IO Data
run [] d = return d
run (Inc : is) d
  = run is (mapPtr (+1) d)
run (Dec : is) d
  = run is (mapPtr (+(-1)) d)
run (PointLeft : is) d
  = run is (moveLeft d)
run (PointRight : is) d
  = run is (moveRight d)
run (Print : is) d
  = putChar (chr $ fromIntegral $ getPtr d) >> run is d
run (Read : is) d 
  = getChar >>= (\c -> run is (putPtr d (fromIntegral $ ord c)))
run is@(Block b : is') d
  | getPtr d == 0 = run is' d
  | otherwise     = run b d >>= run is
run (_ : is) d = run is d

main :: IO ()
main = void $ repl emptyData ""

repl :: Data -> String -> IO Data
repl d s = do
            result <- run (buildAST s) d
            putStrLn ""
            putStr ">> "
            input  <- getLine 
            repl result input
