module AchtungMinen.Game where

import AchtungMinen.World

data Result = Res { won_game :: Bool
                  , mines_found :: Integer
                  , coords_cleared :: [Coord]
                  }
            deriving (Show)

play :: Player p => p -> IO Result
play _ = return (Res False 0 [])
