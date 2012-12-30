module AchtungMinen.Players.DumbAI 
( player
) where
  
import AchtungMinen.World

data DumbPlayer = DP
  
player :: IO DumbPlayer
player = return DP

instance Player DumbPlayer where
  sendMove p = return (p, Try (1, 1))
  recvResp _ p = return p