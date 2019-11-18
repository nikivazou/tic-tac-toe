module Player.Isaac (playerIsaac) where

import Types


teamMembers :: String 
teamMembers = "Isaac Lacort Magan"

playerIsaac :: Player
playerIsaac = Player scoredMoves "Isaac"

-- I asign two scores to each valid Move.
-- The first score is the maximum number of my lined tiles adjacent
-- to a specific move.
-- The second score is the maximum number of rival lined tiles adjacent
-- to a specific move.

-- First I check if my maximum score is 4, in that case I return that
-- move and win the game.
-- If both my score and the rival's score are 0 then this is the first movement,
-- so I return an arbitrary movement.
-- If the score of my rival is equal to or greater that mine, then I return
-- "ribtmv" (rival better movement). This movement has my maximum score
-- among the movements with rival's maximum score 
-- Otherwise I return "mybtmv" (my better movement). This movement has the
-- rival's maximum score among the movements with my maximum score
scoredMoves :: Tile -> Board -> IO Move
scoredMoves tile board
    | mysc == 4              = return mymv
    | mysc == 0 && risc == 0 = return (8,8)
    | risc >= mysc           = return ribtmv
    | otherwise              = return mybtmv
    where
      moves = validMoves board
      ((mysc, mymv):mys)
          = maxTuple (zip (map (scoreTile tile board) moves) moves) [(-1, (-1,-1))]
      ((risc, rimv):ris)
          = maxTuple (zip (map (scoreTile (flipTile tile) board) moves) moves) [(-1, (-1,-1))]
      (mybtsc, mybtmv)
          = maximum $ map (betterScore (flipTile tile) board) ((mysc, mymv):mys)
      (ribtsc, ribtmv)
          = maximum $ map (betterScore tile board) ((risc, rimv):ris)

-- This method returns tuples (i,j) with the constants that have to be added to
-- the current movement in order to reach de adjacent tiles
-- The tiles corresponding to the tuples in the list is the following:
-- |  0  |  1  |  2  |
-- |  3  |     |  4  |
-- |  5  |  6  |  7  |
adjacentTile :: [(Int,Int)]
adjacentTile = [(i,j) | i <- [-1,0,1], j <- [-1,0,1], (i,j) /= (0,0)]

-- This method recalculates the tuple score (Int, Move) by adding the 'tile'
-- score of that movement
betterScore :: Tile -> Board -> (Int, Move) -> (Int, Move)
betterScore tile board (score, move) = (score + (scoreTile tile board move), move)

-- This method returns the list of tuples (score, Move) with maximum score
maxTuple :: [(Int, Move)] -> [(Int, Move)] -> [(Int, Move)]
maxTuple [] result = result 
maxTuple ((score, x):xs) ((max, y):ys)
    | score > max  = maxTuple xs [(score, x)]
    | score == max = maxTuple xs ((score, x):((max, y):ys))
    | otherwise    = maxTuple xs ((max, y):ys)

-- This method returns a movement's score
-- For doing so, it calculates the number of tiles followed in one direction 
-- And finally adds the scores in the same direction but reversed
-- e.g. if this is my board:
-- |  X  |     |     |      The score of the centre (x,y) tile is  
-- |     |     |     |      the number of tile followed in (+1,+1) direction
-- |     |     |  X  |      plus the number of tile followed in (-1,-1) direction
-- So In this case the score for   X   tile is 2
scoreTile :: Tile -> Board -> Move -> Int
scoreTile tile board move
    = maximum $ take 4 $ zipWith (+) scores $ reverse scores
    where
      scores = map (scoreTileRec tile board move) adjacentTile

-- This method returns the number of tiles followed in a given direction
scoreTileRec :: Tile -> Board -> Move -> (Int,Int) -> Int
scoreTileRec tile board (x,y) (i,j)
    | elem ((x+i,y+j), tile) board = 1 + (scoreTileRec tile board (x+i,y+j) (i,j))
    | otherwise                    = 0
