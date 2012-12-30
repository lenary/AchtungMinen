module AchtungMinen.World where

type Coord = (Int, Int)

data Move = Try Coord
          | Mined Coord
          deriving (Show)

data Response = Safe [(Coord, Int)]
              | Dead
              deriving (Show)

class Player p where
  -- |'sendMove' asks the player for their next move
  sendMove :: p -> IO (p, Move)
  -- |'recvResp' lets the player know what happened
  recvResp :: Response -> p -> IO p

maxCoord :: Coord
maxCoord = (10, 10)

mineCount :: Int
mineCount = 10

x, y :: Coord -> Int
x = fst
y = snd

allCoords :: [Coord]
allCoords = [(x', y') | x' <- [1..(x maxCoord)],
                        y' <- [1..(y maxCoord)]]

neighbours :: Coord -> [Coord]
neighbours c = [ (x',y') | x' <- [(x c - 1)..(x c + 1)]
                         , y' <- [(y c - 1)..(y c + 1)]
                         , (x',y') /= c
                         ]

