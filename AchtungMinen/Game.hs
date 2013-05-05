{-# LANGUAGE GADTs #-}

module AchtungMinen.Game where

import AchtungMinen.World

import Control.Applicative
import Control.Monad.State
import qualified Control.Monad.Random as R
import qualified Data.Map as M
import qualified Data.List as L

-- * Some Types
type Map a = M.Map Coord a

-- | A Square either Contains a Mine, or has N mines surrounding it
data Square = Mine
            | Clue Int
            deriving (Show,Eq)

-- | A Field is a Map of Coordinates to Mines or Clues
type Field = Map Square

-- | A Mask is a Map of Coordinates to Booleans
type Fog  = Map Bool

type Result = (Bool, Int, Int)

-- * Public API

-- | Plays a player against a field
play :: Player p => p -> IO Result
play p = do
  st <- newTrial $ newContestant p
  ft <- snd <$> runStateT playTrial st
  return $ scoreTrial ft

-- | Gives a score to the result from `play`
score :: Result -> Int
score (won, correct, incorrect) = (2 * correct - incorrect) * (if won then 3 else 1)

-- * Minefields
-- ** Generation

-- | Creates a (mine)field empty of mines
emptyField :: Field
emptyField = M.fromList [ (c,Clue 0) | c <- allCoords ]

-- | Creates a (mine)field with randomly placed mines and correctly calculated clues
genField :: IO Field
genField = genClues <$> genMines emptyField

-- | Adds `mineCount` mines to a field, randomly placing them
genMines :: Field -> IO Field
genMines f = iterate addMine (pure f) !! mineCount

-- | Updates clues to a field already containing mines
genClues :: Field -> Field
genClues f = M.mapWithKey (addClue f) f

-- | Adds a single mine to a field
addMine :: IO Field -> IO Field
addMine iof = do
  f <- iof
  c <- randomFromList $ safeSquares f
  return $ M.insert c Mine f

-- | Adds a single clue to a field at Coord
addClue :: Field -> Coord -> Square -> Square
addClue _ _ Mine = Mine
addClue f c _    = Clue $ countMinedNeighbours f c

-- | Count neighbour squares that contain mines
countMinedNeighbours :: Field -> Coord -> Int
countMinedNeighbours f c =
  length
  $ minedSquares f `L.intersect` neighbours c

safeSquares, minedSquares :: Field -> [Coord]
safeSquares  = M.keys . M.filter (/= Mine)
minedSquares = M.keys . M.filter (== Mine)

-- | Get a random element from a list
randomFromList :: [a] -> IO a
randomFromList lst = R.fromList $ map (\e -> (e, 1)) lst

-- ** Revealing Logic

revealSquares :: Field -> Fog -> Coord -> ([(Coord, Int)], Fog)
revealSquares = undefined -- TODO

reveal :: Field -> Coord -> Fog -> Fog
reveal f c m = keepHoles m newFog
  where
    newFog = makeFog $ revealOne f c

revealOne :: Field -> Coord -> [Coord]
revealOne f c = revealOne' f [c] []

revealOne' :: Field -> [Coord] -> [Coord] -> [Coord]
revealOne' f []            found = found
revealOne' f (search:rest) found | isEmpty f search = revealOne' f (rest ++ more) (search:found)
                                 | otherwise        = revealOne' f rest           (search:found)
  where
    more = [c | c <- neighbours search
              , c `notElem` found
              , c `notElem` rest
              , c `elem` allCoords
              ]

-- ** Masks

makeFog :: [Coord] -> Fog
makeFog cs = M.fromList [(c,c`elem`cs) | c <- allCoords]

holeyFog, fullFog :: Fog
holeyFog = M.fromList [(c,True)  | c <- allCoords]
fullFog  = M.fromList [(c,False) | c <- allCoords]

keepHoles, keepFilled :: Fog -> Fog -> Fog
-- | Where there are holes, keep them
keepHoles = M.unionWith (||)
-- | Where there are filled squares, keep them
keepFilled = M.unionWith (&&)

holes, filled :: Fog -> [Coord]
holes  = M.keys . M.filter (id)
filled = M.keys . M.filter (not)

-- ** Printing

printField :: Field -> Fog -> IO ()
printField f m =
  putStr
  $ delinate (x maxCoord + 2)
  [ shSq (M.lookup (x',y') m) (M.lookup (x',y') f)
    | y' <- [0..(y maxCoord + 1)]
    , x' <- [0..(x maxCoord + 1)]
    ]
  where
    shSq Nothing      _ = '+'
    shSq (Just False) _ = '#'
    shSq _            Nothing = '+'
    shSq _            (Just Mine) = '*'
    shSq _            (Just (Clue 0)) = ' '
    shSq _            (Just (Clue n)) = head (show n)
    delinate  _  [] = []
    delinate len xs = let (before,after) = splitAt len xs
                      in before ++ "\n" ++ delinate len after

-- * Move-based functions

isMine, isClue, isEmpty :: Field -> Coord -> Bool
isMine  f c = M.lookup c f == (Just Mine)
isClue  f c = not $ isMine f c
isEmpty f c = M.lookup c f == (Just (Clue 0))

isInBoard :: Coord -> Bool
isInBoard c = c `elem` allCoords

-- * Contestant Functions

data Contestant where
  C :: Player p => p -> Contestant

instance Player Contestant where
  sendMove (C p) = sendMove p >>= \(p', m) -> return (C p', m)
  recvResp r (C p) = C <$> recvResp r p

newContestant :: Player p => p -> Contestant
newContestant p = C p

-- * Trial Functions

data Trial = Trial
    { player :: Contestant
    , field  :: Field
    , fog    :: Fog
    , flags  :: [Coord]
    , dead   :: Bool -- ?
    , movesLeft :: Int -- ?
    }

type TrialM = StateT Trial IO

-- | Starts a new trial with a given contestant
newTrial :: Contestant -> IO Trial
newTrial c = do
  f <- genField
  return Trial
    { player = c
    , field  = f
    , fog    = fullFog
    , flags  = []
    , dead   = False
    , movesLeft = (x maxCoord) * (y maxCoord)
    }

-- | Play a game until it finishes
playTrial :: TrialM ()
playTrial = do
  moves_left <- gets movesLeft
  is_dead <- gets dead
  all_done <- flaggedAll
  if is_dead || all_done || moves_left <= 0
    then return ()
    else playTurn >> playTrial

-- | Get the player's next move, see what happens
playTurn :: TrialM ()
playTurn = do
  move <- getNextMove
  resp <- case move of
            (Flag c) -> flagCoord c
            (Clear c) -> clearCoord c
  deliverResponse resp



-- | Get the player's next move
getNextMove :: TrialM Move
getNextMove = do
  t <- get
  (p, m) <- liftIO $ sendMove $ player t
  put t { player = p }
  return m

-- | Send the response to a move to a player
deliverResponse :: Response -> TrialM ()
deliverResponse resp = do
  t <- get
  p' <- liftIO $ recvResp resp $ player t
  put t { player = p'
        , movesLeft = (movesLeft t) - 1
        }

-- | Flag a given coordinate
flagCoord :: Coord -> TrialM Response
flagCoord c = do
  liftIO $ print ("Flagged", c)
  t <- get
  put t { flags = L.nub (c:flags t) }
  return Flagged

-- | Have they flagged all the mines (extra flags are ok)
flaggedAll :: TrialM Bool
flaggedAll = do
  correct <- gets correctFlaggings
  return (correct == mineCount)

-- | The player is attempting to clear `c` - they'll die or they'll reveal more squares
clearCoord :: Coord -> TrialM Response
clearCoord c = do
  field <- gets field
  if c `elem` minedSquares field
    then playerDied
    else revealField c

-- | The player died. Get the mines to return to the player
playerDied :: TrialM Response
playerDied = do
  liftIO $ print "Died"
  t <- get
  put t { dead = True }
  let mines = minedSquares (field t)
  return (Died mines)

-- | The player has not died: reveal more squares
revealField :: Coord -> TrialM Response
revealField c = do
  liftIO $ print ("Revealed", c)
  t <- get
  let (cleared, fog') = revealSquares (field t) (fog t) c
  put t { fog = fog' }
  return (Cleared cleared)

-- | Convert the result of a trial into something `score` will understand
scoreTrial :: Trial -> Result
scoreTrial t = (dead t, correctFlaggings t, incorrectFlaggings t)

-- | Counts the number of squares the player flagged successfully
correctFlaggings, incorrectFlaggings :: Trial -> Int
correctFlaggings t = length $ flags t `L.intersect` minedSquares (field t)
incorrectFlaggings t = length $ flags t L.\\ minedSquares (field t)
