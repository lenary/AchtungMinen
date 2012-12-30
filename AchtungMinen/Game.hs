module AchtungMinen.Game 
( play
) where

import AchtungMinen.World

import Control.Applicative
import Control.Monad
import qualified Control.Monad.Random as R
import qualified Data.Map as M


data Result = Res { won_game :: Bool
                  , mines_found :: Int
                  , coords_cleared :: [Coord]
                  }
            deriving (Show)
            
type Map a = M.Map Coord a

data Square = Mine 
            | Clue Int
            deriving (Show,Eq)

type Board = Map Square
type Fog   = Map Bool

play :: Player p => p -> IO Result
play _ = return (Res False 0 [])

emptyBoard :: Board
emptyBoard = M.fromList [ (c,Clue 0) | c <- allCoords ]

genBoard :: IO Board
genBoard = genClues <$> genMines mineCount (pure emptyBoard)

genClues :: Board -> Board
genClues b = M.mapWithKey (addClue b) b

genMines :: Int -> IO Board -> IO Board
genMines mc ib =
  iterate addMine ib !! mc

addMine :: IO Board -> IO Board
addMine ib = do
  b <- ib
  c <- randomFromList $ freeSquares b
  return $ M.insert c Mine b
  
addClue :: Board -> Coord -> Square -> Square
addClue _ _ Mine = Mine
addClue b c _    = Clue $ countMinedNeighbours b c
  
randomFromList :: [a] -> IO a
randomFromList lst = R.fromList $ map (\x -> (x, 1)) lst
  
freeSquares :: Board -> [Coord]
freeSquares = M.keys . M.filter (== Clue 0)

countMinedNeighbours :: Board -> Coord -> Int
countMinedNeighbours b c = 
  length 
  . (filter (== Mine)) 
  $ [ M.findWithDefault (Clue 0) c' b | c' <- neighbours c ]

printBoard :: Board -> IO ()
printBoard b =
  putStr 
  $ delinate (y maxCoord + 2) 
  [ shSq (M.lookup (x',y') b) 
    | y' <- [0..(y maxCoord + 1)]
    , x' <- [0..(x maxCoord + 1)]
    ]
  where
    shSq Nothing = '+'
    shSq (Just Mine) = '@'
    shSq (Just (Clue 0)) = ' '
    shSq (Just (Clue x)) = head (show x)
    delinate _ [] = []
    delinate len xs = let (before,after) = splitAt len xs in before ++ "\n" ++ delinate len after