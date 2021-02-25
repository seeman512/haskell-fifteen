import System.IO
import System.Random
import Data.List   (elemIndex)
import Text.Printf (printf)
import Data.Maybe  (isNothing, fromJust)

type Position = (Int, Int)

data Game = Game {game  :: [Int],
                  width :: Int} deriving (Eq)

instance Show Game where
    show (Game xs w) = "\n" ++ line ++ foldr showGame "" xs
        where line = replicate (w * 4 + 1) '-' ++ "\n"
              ind num = fromJust (elemIndex num xs) + 1
              nl num = if rem (ind num) w == 0 then "|\n" ++ line else ""
              chNum num = if num == w*w then " " else show num
              showGame num acc = (printf "|%3s" (chNum num)) ++ (nl num) ++ acc

data FDirection = FLeft | FRight | FTop | FBottom
                  deriving (Show, Eq, Enum, Bounded)

instance Random FDirection where
    randomR (a, b) g = (toEnum x, g')
        where (x, g') = randomR (fromEnum a, fromEnum b) g
    random = randomR (minBound, maxBound)

getGame :: Int -> Game
getGame w = Game [1 .. w * w] w

replaceElements :: Int -> Int -> Game -> Game
replaceElements x1 x2 g@(Game xs w)
    | x1 == x2                    = g
    | isNothing (elemIndex x1 xs) = g
    | isNothing (elemIndex x2 xs) = g
    | otherwise = 
        let
            i1 = fromJust $ elemIndex x1 xs
            i2 = fromJust $ elemIndex x2 xs
        in
            if i1 > i2 then replaceElements x2 x1 g 
            else Game (prev' ++ [x2] ++ prev'' ++ [x1] ++ (tail rest'')) w
                    where (prev',  rest')  = break (==x1) xs
                          (prev'', rest'') = break (==x2) $ tail rest'

getPosByNum :: Int -> Game -> Position
getPosByNum n g@(Game xs w)
    | isNothing (elemIndex n xs) = (0,   0)
    | otherwise                  = (col, row)
        where i = fromJust $ elemIndex n xs
              row = div i w
              col = i - row * w

getNumByPos :: Position -> Game -> Int
getNumByPos (col, row) (Game xs w) = xs !! (row * w + col)

getNextPos :: FDirection -> Position -> Game -> Position
getNextPos d pos@(col, row) g@(Game _ w) = 
    case d of
        FRight  -> if col == 0     then pos else (col - 1, row) 
        FLeft   -> if col == w - 1 then pos else (col + 1, row) 
        FBottom -> if row == 0     then pos else (col,     row - 1)
        FTop    -> if row == w - 1 then pos else (col,     row + 1)

move :: FDirection -> Game -> Game
move d g@(Game xs w) =
    let num = w * w
        pos = getPosByNum num g
        nextPos = getNextPos d pos g
        nextNum = getNumByPos nextPos g
    in
        replaceElements num nextNum g

gameLoop :: Game -> IO ()
gameLoop g = do
    putStr "\ESC[2J"
    putStr $ show g
    let winGame = getGame $ width g
    if g == winGame then putStrLn "You are win!"
    else do
        ch <- getChar
        case ch of
            'w' -> gameLoop $ move FTop    g
            's' -> gameLoop $ move FBottom g
            'a' -> gameLoop $ move FLeft   g
            'd' -> gameLoop $ move FRight  g
            'q' -> putStrLn "\ESC[2JGame Over"
            _   -> gameLoop g

shuffleGame :: Int -> Game -> IO Game
shuffleGame 0 g = return g
shuffleGame i g = randomIO >>= \d -> shuffleGame (i - 1) (move d g)
            
fgame = getGame 4
iters = 400

main :: IO ()
main = hSetBuffering stdin NoBuffering >> shuffleGame iters fgame >>= gameLoop
