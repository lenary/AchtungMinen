module AchtungMinen.World where

type Coord = (Integer, Integer)

data Move = Try Coord
          | Mined Coord
          deriving (Show)

data Response = Safe [(Coord, Clue)]
              | Dead
              deriving (Show)

data Clue = Empty
          | Near Integer
          deriving (Show)

class Player p where
  -- |'sendMove' asks the player for their next move
  sendMove :: p -> IO (p, Move)
  -- |'recvResp' lets the player know what happened
  recvResp :: Response -> p -> IO p

maxCoord :: Coord
maxCoord = (10, 10)

x, y :: Coord -> Integer
x = fst
y = snd

allCoords :: [Coord]
allCoords = [(x', y') | x' <- [1..(x maxCoord)],
                        y' <- [1..(y maxCoord)]]

