{-# LANGUAGE ExistentialQuantification, FlexibleInstances #-}

module AchtungMinen.Game2 where

import Control.Monad.IO.Class
import Control.Monad.Trans.State

type Coord = (Int, Int)
data Move = Clear Coord | Abort deriving (Show, Eq)
data Response = Clue Int deriving (Show, Eq)

-- What do we want?
-- a) a simple player interface, with two (impure) functions: `move` and `hear`
-- b) a simple internal object to wrap current game and player info

class (MonadIO p) => Player p where
    playerMove :: p Move
    playerHear :: Response -> p ()

data AbortPlayer = AbortPlayer deriving (Show)
instance Player IO where
    playerMove = return Abort
    playerHear _ = return ()

type StatefulPlayer = StateT () IO

mkStatefulPlayer = ()
instance Player StatefulPlayer where
    playerMove = return Abort
    playerHear _ = return ()

