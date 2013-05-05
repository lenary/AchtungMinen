-- | Utility functions? Never!
module AchtungMinen.World where

-- | A coordinate in 2d space
type Coord = (Int, Int)

-- | What is the player's next move?
data Move = Clear Coord
          | Flag Coord
          deriving (Show)

-- | How did your last move go?
--
-- Did you flag something? Did you clear some spaces?
data Response = Flagged
              | Cleared [(Coord, Int)]
              | Died [Coord]
              deriving (Show)

-- | How did the game end?
--
-- Did you die, how many flags were correct, how many incorrect?
data Result = Result
              { alive :: Bool
              , flaggedCorrectly :: Int
              , flaggedInCorrectly :: Int
              } deriving (Show)

-- | All players should implement this typeclass in order to play
class Player p where
  -- | 'sendMove' asks the player for their next move
  sendMove :: p -> IO (p, Move)
  -- | 'recvResp' lets the player know what happened
  recvResp :: Response -> p -> IO p

-- | The board is this size (`fst` * `snd`) in squares
maxCoord :: Coord
maxCoord = (9, 9)

-- | There are this many mines to be found each game
mineCount :: Int
mineCount = 10

x, y :: Coord -> Int
-- | Retrieve the x component of a coordinate
x = fst
-- | Retrieve the y component of a coordinate
y = snd

-- | What are all the coordinates in the game?
allCoords :: [Coord]
allCoords = [(x', y') | x' <- [1..(x maxCoord)],
                        y' <- [1..(y maxCoord)]]

-- | Which are the coordinates of my current square (doesn't respect boundaries)
neighbours :: Coord -> [Coord]
neighbours c = [ (x',y') | x' <- [(x c - 1)..(x c + 1)]
                         , y' <- [(y c - 1)..(y c + 1)]
                         , (x',y') /= c
                         ]

