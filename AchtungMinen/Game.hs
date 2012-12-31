module AchtungMinen.Game 
( play
, score
) where

import AchtungMinen.World

import Control.Applicative
import Control.Monad
import qualified Control.Monad.Random as R
import qualified Data.Map as M
import qualified Data.List as L


data Result = Res { won_game :: Bool
                  , mines_found :: Int
                  , coords_cleared :: Int
                  }
            deriving (Show)
            
type Map a = M.Map Coord a

data Square = Mine 
            | Clue Int
            deriving (Show,Eq)

type Board = Map Square
type Mask  = Map Bool

play :: Player p => p -> IO Result
play _ = return (Res False 0 0)

score :: Result -> Int
score r = (coords_cleared r) + (10 * mines_found r) + (if won_game r then 100 else 0)

emptyBoard :: Board
emptyBoard = M.fromList [ (c,Clue 0) | c <- allCoords ]

genBoard :: IO Board
genBoard = genClues <$> genMines emptyBoard

genMines :: Board -> IO Board
genMines b = iterate addMine (pure b) !! mineCount
  
genClues :: Board -> Board
genClues b = M.mapWithKey (addClue b) b

addMine :: IO Board -> IO Board
addMine ib = do
  b <- ib
  c <- randomFromList $ safeSquares b
  return $ M.insert c Mine b
  
addClue :: Board -> Coord -> Square -> Square
addClue _ _ Mine = Mine
addClue b c _    = Clue $ countMinedNeighbours b c
  
randomFromList :: [a] -> IO a
randomFromList lst = R.fromList $ map (\x -> (x, 1)) lst
  
safeSquares :: Board -> [Coord]
safeSquares = M.keys . M.filter (/= Mine)

countMinedNeighbours :: Board -> Coord -> Int
countMinedNeighbours b c = 
  length 
  . (filter (== Mine))
  $ [ M.findWithDefault (Clue 0) c' b | c' <- neighbours c ]

-- From here to END needs a rewrite to become faster. Ask pfkh
reveal :: Board -> Coord -> [Coord]
reveal b start | isEmpty b start = L.nub $ revealEmptyNeighbours b $ revealEmpty b start
               | otherwise       = [start]
  
isEmpty :: Board -> Coord -> Bool
isEmpty b c = M.lookup c b == (Just (Clue 0))

revealEmpty :: Board -> Coord -> [Coord]
revealEmpty b c = revealEmpty' b [c] [] []

revealEmpty' _ [] empty _ = empty
revealEmpty' b (tc:rest) empty checked | isEmpty b tc = revealEmpty' b (more ++ rest) (tc:empty) (tc:checked)
                                       | otherwise    = revealEmpty' b  rest           empty     (tc:checked)
  where
    more = [c | c <- pathNeighbours tc
              , c `elem` allCoords
              , c `notElem` checked
              ]

revealEmptyNeighbours :: Board -> [Coord] -> [Coord]
revealEmptyNeighbours b es = 
  [n | e <- es
     , n <- [e] ++ pathNeighbours e
     , n `elem` allCoords
     ]

pathNeighbours :: Coord -> [Coord]
pathNeighbours (x,y) = [(x,y+1),(x,y-1),(x+1,y),(x-1,y)]
-- END

printBoard :: Board -> IO ()
printBoard = flip printWithOverlay $ holeyMask

-- Masks

makeMask :: [Coord] -> Mask
makeMask cs = M.fromList [(c,c`elem`cs) | c <- allCoords]

holeyMask, fullMask :: Mask
holeyMask = M.fromList [(c,True)  | c <- allCoords]
fullMask  = M.fromList [(c,False) | c <- allCoords]

printMask :: Mask -> IO ()
printMask = printWithOverlay emptyBoard
    
printWithOverlay :: Board -> Mask -> IO ()
printWithOverlay b m =
  putStr
  $ delinate (y maxCoord + 2)
  [ shSq (M.lookup (x',y') m) (M.lookup (x',y') b) 
    | y' <- [0..(y maxCoord + 1)]
    , x' <- [0..(x maxCoord + 1)]
    ]
  where
    shSq Nothing _ = '+'
    shSq (Just False) _ = '#'
    shSq _            (Just Mine) = '*'
    shSq _            (Just (Clue 0)) = ' '
    shSq _            (Just (Clue x)) = head (show x)
    delinate _ [] = []
    delinate len xs = let (before,after) = splitAt len xs 
                      in before ++ "\n" ++ delinate len after
