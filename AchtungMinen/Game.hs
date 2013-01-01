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
            
data GameState = InProgress { field :: Field
                            , fog :: Mask
                            }
               | Finished { won_game :: Bool
                          , mines_found :: Int
                          , coords_cleared :: Int
                          }
               deriving (Show)
            
type Map a = M.Map Coord a

data Square = Mine 
            | Clue Int
            deriving (Show,Eq)

type Field = Map Square
type Mask  = Map Bool

-- Public API

play :: Player p => p -> IO (Bool, Int, Int)
play _ = return (False, 0, 0)

score :: (Bool, Int, Int) -> Int
score (won, found, cleared) = cleared + 10 * found + (if won then 100 else 0)

-- Field Generation

emptyField :: Field
emptyField = M.fromList [ (c,Clue 0) | c <- allCoords ]

genField :: IO Field
genField = genClues <$> genMines emptyField

genMines :: Field -> IO Field
genMines b = iterate addMine (pure b) !! mineCount
  
genClues :: Field -> Field
genClues b = M.mapWithKey (addClue b) b

addMine :: IO Field -> IO Field
addMine ib = do
  b <- ib
  c <- randomFromList $ safeSquares b
  return $ M.insert c Mine b
  
addClue :: Field -> Coord -> Square -> Square
addClue _ _ Mine = Mine
addClue b c _    = Clue $ countMinedNeighbours b c
  
randomFromList :: [a] -> IO a
randomFromList lst = R.fromList $ map (\x -> (x, 1)) lst

countMinedNeighbours :: Field -> Coord -> Int
countMinedNeighbours b c = 
  length 
  $ minedSquares b `L.intersect` neighbours c

-- Revealing Logic

reveal :: Field -> Coord -> Mask
reveal b start = reveal' b [start] fullMask

reveal' :: Field -> [Coord] -> Mask -> Mask
reveal' _ []            found = found
reveal' b (search:next) found | isEmpty b search = reveal' b (more ++ next) (M.insert search True found)
                              | otherwise        = reveal' b next           (M.insert search True found)
  where
    more = [c | c <- neighbours search
              , c `notElem` M.keys (M.filter id found)
              , c `notElem` next
              , c `elem` allCoords
              ]

isEmpty :: Field -> Coord -> Bool
isEmpty b c = M.lookup c b == (Just (Clue 0))

-- Masks

makeMask :: [Coord] -> Mask
makeMask cs = M.fromList [(c,c`elem`cs) | c <- allCoords]

holeyMask, fullMask :: Mask
holeyMask = M.fromList [(c,True)  | c <- allCoords]
fullMask  = M.fromList [(c,False) | c <- allCoords]

subtractMasks :: Mask -> Mask -> Mask
subtractMasks = M.unionWith (||)

-- Printing

printField :: Field -> IO ()
printField = flip printBoth $ holeyMask

printOverlay :: Mask -> IO ()
printOverlay = printBoth emptyField
    
printBoth :: Field -> Mask -> IO ()
printBoth b m =
  putStr
  $ delinate (x maxCoord + 2)
  [ shSq (M.lookup (x',y') m) (M.lookup (x',y') b) 
    | y' <- [0..(y maxCoord + 1)]
    , x' <- [0..(x maxCoord + 1)]
    ]
  where
    shSq Nothing      _ = '+'
    shSq (Just False) _ = '#'
    shSq _            (Just Mine) = '*'
    shSq _            (Just (Clue 0)) = ' '
    shSq _            (Just (Clue x)) = head (show x)
    delinate _ [] = []
    delinate len xs = let (before,after) = splitAt len xs 
                      in before ++ "\n" ++ delinate len after

--

safeSquares, minedSquares :: Field -> [Coord]
safeSquares = M.keys . M.filter (/= Mine)
minedSquares = M.keys . M.filter (== Mine)
