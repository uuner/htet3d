module Field where
import Data.List
import Data.Maybe
import System.Random
import qualified Data.Array as A
import qualified Data.Map as M

import Colors

(!) = (A.!)

data Piece = Piece {boxes  :: [[Int]],
                    center :: [Int],
                    pcol   :: PieceColor}

data Field = Field {glass     :: M.Map [Int] (PieceColor),
                    nextPiece :: Piece,
                    fieldW    :: Int,
                    fieldH    :: Int,
                    fieldD    :: Int,
                    scores    :: Integer,
                    eatenrows :: Int,
                    level     :: Int,
                    isOver    :: Bool}

createField :: Int -> Int -> Int -> Piece -> Field
createField width height depth next = 
    Field { glass = M.empty,  
            nextPiece = next,
            fieldW = width,
            fieldH = height,
            fieldD = depth,
            scores = 0,
            eatenrows = 0,
            level = 1,
            isOver = False}

createRandomPiece :: Int -> Int -> Int -> IO Piece
createRandomPiece w h d = do
    rand <- getStdRandom (randomR (0,length allPieces - 1))
    let piece' = allPieces!!rand
    return (piece' {boxes = map (\[x,y,z] -> [x + div w 2, y + div h 2, z + (d-1)]) (boxes piece'),
                    center = (\[x,y,z] -> [x + 2*div w 2, y + 2*div h 2, z + 2*(d-1)]) (center piece')})

scoreTable :: A.Array Int Integer
scoreTable = A.listArray (0,4) [0,100,200,400,800]

maxLevel :: Int
maxLevel = 9

levelTime :: A.Array Int Int
levelTime = A.listArray (1,maxLevel) [1000,700,600,500,400,300,200,150,100]

movePiece :: ([Int] -> [Int]) -> Field -> Piece -> Maybe (Field, Piece)
movePiece f field piece
    | isValid field newpiece = Just (field, newpiece)
    | otherwise = Nothing
    where
    newpiece = piece {boxes = map f (boxes piece), 
                      center = (f.f) (center piece) }

dropDown :: Field -> Piece -> Maybe (Field, Piece)
dropDown field piece = Just (field, newpiece)
    where
    newpiece = drop' (movePiece (\[x,y,z] -> [x,y,z-1]) field) piece
    drop' f p
        | isNothing d = p
        | otherwise = drop' f $ snd $ fromJust d
        where 
        d = f p

dropPiece :: Field -> Piece -> IO (Field, Piece)
dropPiece field piece
    | isJust dropped = return $ fromJust dropped
    | True = do newNext <- createRandomPiece (fieldW field) (fieldH field) (fieldD field)
                let newfield = (freeze field piece) {nextPiece = newNext}
                if (isValid newfield newPiece)
                   then return (newfield, newPiece)
                   else return (newfield {isOver = True}, newPiece)
    where 
    dropped = movePiece (\[x,y,z] -> [x,y,z-1]) field piece
    newPiece = nextPiece field

rotatePiece :: Int -> Int -> Field -> Piece -> Maybe (Field, Piece)
rotatePiece axis dir field piece 
    | isValid field newpiece = Just (field, newpiece)
    | not $ null movings = Just (field, head movings)
    | otherwise = Nothing
    where
    multmatr m v = map (\x -> sum $ zipWith (*) x v) m
    rotmatrs = [[[1,0,0],
                 [0,0,-1],
                 [0,1,0]],
                [[0,0,1],
                 [0,1,0],
                 [-1,0,0]], 
                [[0,-1,0],
                 [1,0,0],
                 [0,0,1]]]
    r = center piece
    np = map (\x -> map (`div` 2) $ 
               zipWith (+) r $ 
               multmatr ((if dir == 0 then id else transpose) rotmatrs!!axis) 
                        (zipWith (-) (map (*2) x) r)) (boxes piece)
    newpiece = piece {boxes = np}
    movings = filter (\x -> isValid field x) $
                map (\f -> newpiece {boxes = map f np}) 
                [(\[x,y,z] -> [x-1,y,z]), 
                 (\[x,y,z] -> [x+1,y,z]), 
                 (\[x,y,z] -> [x,y-1,z]),
                 (\[x,y,z] -> [x,y+1,z])]

isValid :: Field -> Piece -> Bool
isValid field@(Field {glass = m, fieldW = w, fieldH = h, fieldD = d}) 
        peice@(Piece {boxes = xs}) = 
    all (\[x,y,z] -> z >= 0 && x >= 0 && y >= 0 &&
                     x < w && y < h && --z < d && 
                     not (M.member [x,y,z] m)) xs

freeze :: Field -> Piece -> Field
freeze field@(Field {glass = gl, fieldW = w, fieldH = h, fieldD = d, scores = sc}) 
       peice@(Piece {boxes = xs, pcol = col}) =
       field { glass = newglass',
               scores = sc + scoreTable!(length rows),
               eatenrows = newER,
               level = min maxLevel (1 + div newER 5), 
               isOver = any (\[_,_,z] -> z >= d) (M.keys newglass')}
    where 
    newglass = foldl (\m x -> M.insert x col m) gl xs
    rows = findFullRows newglass w h d
    newER = eatenrows field + length rows
    newglass' = M.mapKeys (\[x,y,z] -> [x,y,z - length (filter (<z) rows)]) $
                M.filterWithKey (\[_,_,z] _ -> not (z `elem` rows)) newglass

findFullRows m w h d =
    map head $ filter (\x -> length x == w*h) $ group $ sort $ map last $ M.keys m

allPieces :: [Piece]
allPieces = [Piece {boxes = [[-1,0,0],[0,0,0],[0,-1,0],[1,0,0]],
                     center = [0,0,0],
                     pcol = yellow},
              Piece {boxes = [[0,1,0],[0,-1,0],[0,0,0],[0,2,0]],
                     center = [0,0,0],
                     pcol = pink},
              Piece {boxes = [[0,1,0],[0,0,0],[1,0,0],[1,1,0]],
                     center = [1,1,1],
                     pcol = green},
              Piece {boxes = [[-1,1,0],[0,1,0],[0,0,0],[1,0,0]],
                     center = [0,0,0],
                     pcol = violet}, 
              Piece {boxes = [[-1,1,0],[-1,0,0],[0,0,0],[1,0,0]],
                     center = [0,0,0],
                     pcol = red}]


