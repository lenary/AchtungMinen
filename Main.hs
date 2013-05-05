module Main where

import qualified AchtungMinen.Game as AM
import qualified AchtungMinen.Players.DumbAI as P

main :: IO ()
main = do
  p <- P.player
  res <- AM.play p
  print ("Quack!", AM.score res, res)