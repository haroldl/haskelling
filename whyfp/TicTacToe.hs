{-# LANGUAGE NoMonomorphismRestriction, MultiParamTypeClasses #-}

{-
   Tic Tac Toe, based on Why Functional Programming Matters
   http://www.cs.kent.ac.uk/people/staff/dat/miranda/whyfp90.pdf
 -}

import Data.List
import Data.Maybe
import Data.Tree

class Position p m where
  moves :: p -> [p]
  playersTurn :: p -> m
  winner :: p -> m
  staticEvaluation :: p -> Double

--- Tic Tac Toe data structures

data Mark = X | O | Neither
  deriving (Eq, Ord, Read, Show)

data TicTacToePosition = TicTacToePosition [Mark]

instance Position TicTacToePosition Mark where
  moves = tttMoves
  playersTurn = tttTurn
  winner = tttWinner
  staticEvaluation _ = 0

tttMoves (TicTacToePosition nums) =
    let indices = findIndices (== Neither) nums in
      map (\i -> TicTacToePosition $ replace nums X i) indices
  where replace arr new n = let (as, bs) = splitAt n arr in
                              as ++ (new : tail bs)

tttTurn (TicTacToePosition marks) =
    let plays = filter (/= Neither) (X : O : marks) in
      mostCommonValue (group $ reverse $ sort $ plays)
  where compare' (_, a) (_, b) = compare a b
        countDistinct = map (\xs@(x:_) -> (x, length xs))
        mostCommonValue = fst . head . sortBy compare' . countDistinct

tttWinner (TicTacToePosition marks) = 
    let wins = catMaybes $ map winner (map (map (marks !!)) indices) in
      if length wins > 0 then head wins else Neither
  where indices = map (\i -> [i, i+3, i+6]) [0, 1, 2] ++
                  map (\i -> [i, i+1, i+2]) [0, 3, 6] ++
                  [[0, 5, 8], [2, 5, 7]]
        winner (a:b:c:[]) = if a == b && b == c && a /= Neither
                              then Just a
                              else Nothing

-- Trees

reptree fn val = Node val (map (reptree fn) (fn val))

omit _ _ [] = []
omit seekMax potential numslists = omit' potential numslists
  where minmax = if seekMax then minimum else maximum
        test n = if seekMax then n <= potential else n >= potential
        omit' potential (nums:rest) =
          if any test nums
            then omit' potential rest
            else let n = minmax nums in n : omit' n rest

maximize' (Node v []) = [v]
maximize' (Node v children) = mapmin (map minimize' children)
  where mapmin (nums:rest) = minimum nums : omit True (minimum nums) rest

minimize' (Node v []) = [v]
minimize' (Node v children) = mapmax (map maximize' children)
  where mapmax (nums:rest) = maximum nums : omit False (maximum nums) rest

maximize = maximum . maximize'
minimize = minimum . minimize'

prune 0 (Node v _) = Node v []
prune n (Node v children) = Node v (map (prune (n-1)) children)

highfirst (Node n sub) = Node n (sortBy higher (map lowfirst sub))
  where higher (Node n1 _) (Node n2 _) = compare n2 n1
lowfirst (Node n sub) = Node n (sortBy lower (map highfirst sub))
  where lower (Node n1 _) (Node n2 _) = compare n1 n2

-- Game trees

gametree pos = reptree moves pos

evaluate = maximize . highfirst . fmap staticEvaluation . prune 5 . gametree
