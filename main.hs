import Data.Char
import Data.Maybe 
import Control.Monad
import Data.Word (Word8)

data Zip a = Zip [a] a [a] deriving (Show)

data BFInst = Inc 
            | Dec 
            | PointLeft 
            | PointRight 
            | LoopOpen 
            | LoopClose 
            | Print 
            | Read
            | Comment Char
            | Debug 
            deriving (Show)

type Data   = Zip Word8
type Source = Zip BFInst

emptyData :: Data
emptyData = Zip (repeat 0) 0 (repeat 0)

------------------------------------

mapPtr :: (a -> a) -> Zip a -> Zip a
mapPtr f (Zip l x r)
  = Zip l (f x) r

getPtr :: Zip a -> a
getPtr (Zip _ p _) = p

putPtr :: Zip a -> a -> Zip a
putPtr (Zip l _ r) x = Zip l x r

moveLeft :: Zip a -> Maybe (Zip a)
moveLeft (Zip [] _ _)
  = Nothing
moveLeft (Zip (l : ls) p rs)
  = Just $ Zip ls l (p : rs)

moveRight :: Zip a -> Maybe (Zip a)
moveRight (Zip _ _ [])
  = Nothing
moveRight (Zip ls p (r : rs))
  = Just $ Zip (p : ls) r rs

matchingLoopClose :: Source -> Maybe Source
matchingLoopClose = matchingLoopClose' 0

matchingLoopClose' :: Int -> Source -> Maybe Source
matchingLoopClose' 1 s@(Zip _ LoopClose _)
  = Just s
matchingLoopClose' b s@(Zip _ LoopOpen _)
  = moveRight s >>= matchingLoopClose' (b + 1)
matchingLoopClose' b s@(Zip _ LoopClose _)
  = moveRight s >>= matchingLoopClose' (b - 1)
matchingLoopClose' b s
  = moveRight s >>= matchingLoopClose' b

matchingLoopOpen :: Source -> Maybe Source
matchingLoopOpen = matchingLoopOpen' 0

matchingLoopOpen' :: Int -> Source -> Maybe Source
matchingLoopOpen' 1 s@(Zip _ LoopOpen _)
  = Just s
matchingLoopOpen' b s@(Zip _ LoopClose _)
  = moveLeft s >>= matchingLoopOpen' (b + 1)
matchingLoopOpen' b s@(Zip _ LoopOpen _)
  = moveLeft s >>= matchingLoopOpen' (b - 1)
matchingLoopOpen' b s
  = moveLeft s >>= matchingLoopOpen' b
------------------------------------
parseChar :: Char -> BFInst
parseChar '+' = Inc
parseChar '-' = Dec
parseChar '<' = PointLeft
parseChar '>' = PointRight
parseChar '[' = LoopOpen
parseChar ']' = LoopClose
parseChar '.' = Print
parseChar ',' = Read
parseChar '#' = Debug
parseChar c   = Comment c

parseString :: String -> Maybe Source
parseString [] = Nothing
parseString s  = Just $ Zip [] h t
  where (h : t) = map parseChar s

run' :: Data -> Maybe Source -> IO Data

run' d Nothing 
  = return d

run' d@(Zip _ p _) s@(Just (Zip _ cmd _)) = case (cmd, p) of
  (Inc       , _) -> run' (mapPtr (+1) d) next
  (Dec       , _) -> run' (mapPtr (+(-1)) d) next
  (Print     , _) -> putChar (chr $ fromIntegral $ getPtr d) >> run' d next
  (PointLeft , _) -> run' (fromJust $ Just d >>= moveLeft) next
  (PointRight, _) -> run' (fromJust $ Just d >>= moveRight) next
  (Debug     , _) -> print (getPtr d) >> run' d next
  (LoopOpen  , 0) -> run' d (s >>= matchingLoopClose)
  (LoopClose , 0) -> run' d next
  (LoopClose , _) -> run' d (s >>= matchingLoopOpen)
  (Read      , _) -> do
                       inp <- getChar
                       run' (putPtr d (fromIntegral $ ord inp)) next
  (_         , _) -> run' d next
  where next = s >>= moveRight
  

main :: IO ()
main = void $ repl emptyData ""

repl :: Data -> String -> IO Data
repl d s = do
            result <- run' d (parseString s)
            putStrLn ""
            putStr ">> "
            input  <- getLine 
            repl result input
