{-# LANGUAGE GADTs #-}

-- | This makes the AchtungMinen World go around
--
-- In here is the magic that turns a simple AI into
-- something that can't actually beat minesweeper.
module AchtungMinen.Game
( play
, score
) where

import AchtungMinen.World

import Control.Applicative
import Control.Monad.State
import qualified Control.Monad.Random as R
import qualified Data.Map as M
import qualified Data.List as L
import Data.Maybe (fromMaybe)

-- * Some Types

-- | Something that plots `a`s on a board of some kind
type Map a = M.Map Coord a

-- | A Field is a Map of Coordinates to Mines or Clues (Squares)
type Field = Map Square

-- | A Square either Contains a Mine, or has N mines surrounding it
data Square = Mine
            | Clue { clueCount :: Int}
            deriving (Show,Eq)

-- | A Mask is a Map of Coordinates to Booleans
type Fog  = Map Bool

-- * Public API

-- | Plays a player against a field
play :: Player p => p -> IO Result
play p = do
  st <- newTrial $ newContestant p
  scoreTrial `liftM` execStateT playTrial st

-- | Gives a score to the result from `play`
score :: Result -> Int
score r = (2 * flaggedCorrectly r)
          + (if flaggedCorrectly r == mineCount then 2 * mineCount else 0)
          - (flaggedInCorrectly r)
          - (if alive r then 0 else 20)

-- * Minefields
-- ** Generation
-- | Creates a (mine)field empty of mines
emptyField :: Field
emptyField = M.fromList [ (c,Clue 0) | c <- allCoords ]

-- | Creates a field with randomly placed mines and correctly calculated clues
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
-- | Squares in the field that are not mined
safeSquares  = M.keys . M.filter (/= Mine)
-- | Squares in the field that are mined. Gulp
minedSquares = M.keys . M.filter (== Mine)

-- | Get a random element from a list
randomFromList :: [a] -> IO a
randomFromList lst = R.fromList $ map (\e -> (e, 1)) lst

-- ** Revealing Logic

-- | Reveals a single square.
--
-- The logic here is tricky.
-- In the case of a mined square: Reveal the mines :)
-- In the case of a square with a nonzero clue: only uncover that square
-- In the case of an empty (0 adjacent mines) clue-square:
--   It will uncover as many "empty" squares as possible in one continuious block
--   (yes, it can jump over corners), and then uncover any clue squares that surround
--   those empty squares. As long as `genClues` has been called on the field, then it
--   won't attempt to uncover mines.
--
-- This returns two things, a list of coordinate -> clue pairs, and the new fog.
reveal :: Field -> Fog -> Coord -> ([(Coord, Int)], Fog)
reveal fi fo c | c `elem` minedSquares fi = ([], revealMines fi fo)
               | c `notElem` allCoords    = ([], fo) -- If only we could give an error here to make them fuck off
               | otherwise                = (revealed, fog') -- TODO
  where
    revealed = revealSquare fi c
    fog' = keepRevealed fo $ makeFog $ map fst revealed

-- | Reveals all the mines from the fog :)
revealMines :: Field -> Fog -> Fog
revealMines fi fo = keepRevealed fo $ makeFog $ minedSquares fi

-- | Reveals clues starting from a single clue square
--
-- Mostly uses the recursive function `revealSquare'`
revealSquare :: Field -> Coord -> [(Coord, Int)]
revealSquare f c = revealSquare' f [c] []

-- | Acutally reveal squares.
--
-- The algorithm is described in `reveal`, this only does clue-based squares though
revealSquare' :: Field -> [Coord] -> [(Coord, Int)] -> [(Coord, Int)]
revealSquare' f []            found = found
revealSquare' f (search:rest) found | isEmpty f search = revealSquare' f (rest ++ more) (clue:found)
                                    | otherwise        = revealSquare' f rest           (clue:found)
  where
    more = [c | c <- neighbours search
              , c `notElem` map fst found
              , c `notElem` rest
              , c `elem` allCoords
              ]
    clue = (search, revealClue f search)

-- | Takes a clue coordinate, and returns what the clue is,
--
-- mostly because we never really look up this data in `revealSquare'`
revealClue :: Field -> Coord -> Int
revealClue f c | isMine f c = error "Don't use revealClue on a mined square"
               | otherwise  = clueCount $ fromMaybe (Clue 0) $ M.lookup c f

-- ** Fogs

-- | Makes a fog with the given coordinates revealed
makeFog :: [Coord] -> Fog
makeFog cs = M.fromList [(c,c`elem`cs) | c <- allCoords]

-- | Makes a fog with no revealed squares
fullFog :: Fog
fullFog  = M.fromList [(c,False) | c <- allCoords]

-- | Merge two fogs, keeping any squares revealed that are revealed in either
keepRevealed :: Fog -> Fog -> Fog
keepRevealed = M.unionWith (||)

-- ** Printing

-- | Prints out a minefield, complete with fog of war
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


isMine, isEmpty :: Field -> Coord -> Bool
-- | Does this coordinate in this field have a mine in it?
isMine  f c = M.lookup c f == (Just Mine)
-- | Does this coordinate in this field have a zero clue in it?
isEmpty f c = M.lookup c f == (Just (Clue 0))

-- | Is this coordinate in the board at all? Best just check
isInBoard :: Coord -> Bool
isInBoard c = c `elem` allCoords

-- * Contestant Functions

-- | GADTs - I still don't quite understand them, but they have been helpful :)
--
-- I think this is needed so that the definition of trial can actually be a record
-- but I could be wrong, and there might be a simpler way.
data Contestant where
  C :: Player p => p -> Contestant

-- | This is just so we can show off a contestant. We may add names later
instance Show Contestant where
  show (C _) = "Contestant!"

-- | This proxies the functions down into their internal contestants
instance Player Contestant where
  sendMove (C p) = sendMove p >>= \(p', m) -> return (C p', m)
  recvResp r (C p) = C <$> recvResp r p

-- | Contestant construction function, for ease of use
newContestant :: Player p => p -> Contestant
newContestant p = C p

-- * Trial Functions

-- | Contains the data from a single trial-by-minesweep
data Trial = Trial
    { player :: Contestant
    , field  :: Field
    , fog    :: Fog
    , flags  :: [Coord]
    , dead   :: Bool -- ?
    , movesLeft :: Int -- ?
    } deriving Show

-- | Oh god, StateTransformers. I think I kinda get them though :)
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
  t <- get
  moves_left <- gets movesLeft
  is_dead <- gets dead
  all_done <- flaggedAll
  if is_dead || all_done || moves_left <= 0
    then printTrial >> return ()
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
  if isMine field c || not (isInBoard c)
    then playerDied
    else revealField c

-- | The player died. Get the mines to return to the player
playerDied :: TrialM Response
playerDied = do
  liftIO $ print "Died"
  t <- get
  let mines = minedSquares (field t)
  let fog' = revealMines (field t) (fog t)
  put t { dead = True, fog = fog' }
  return (Died mines)

-- | The player has not died: reveal more squares
revealField :: Coord -> TrialM Response
revealField c = do
  liftIO $ print ("Revealed", c)
  t <- get
  let (cleared, fog') = reveal (field t) (fog t) c
  put t { fog = fog' }
  return (Cleared cleared)

-- | Print out the current field/fog state
printTrial :: TrialM ()
printTrial = do
  field <- gets field
  fog <- gets fog
  liftIO $ printField field fog

-- | Convert the result of a trial into something `score` will understand
scoreTrial :: Trial -> Result
scoreTrial t = Result
                { alive = not (dead t)
                , flaggedCorrectly = correctFlaggings t
                , flaggedInCorrectly = incorrectFlaggings t
                }

-- | Counts the number of squares the player flagged successfully
correctFlaggings, incorrectFlaggings :: Trial -> Int
correctFlaggings t = length $ flags t `L.intersect` minedSquares (field t)
incorrectFlaggings t = length $ flags t L.\\ minedSquares (field t)
