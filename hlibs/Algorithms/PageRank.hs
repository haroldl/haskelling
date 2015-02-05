-- | Implement the PageRank link analysis algorithm for scoring web pages
-- based on the hyperlink structure between pages.
module Algorithms.PageRank (pageRank) where

import Data.Matrix ((!), matrix, scaleMatrix, nrows, ncols, toList, Matrix)
import Data.Ratio ((%), Ratio)

-- | 'pageRank total beta marix' generates successive approximations of the
--   rank vector with the PageRank score for each web page in a corpus.
--
-- This is guaranteed to converge to a unique result with beta < 1.
-- In practice it will converge very quickly, less than 50 iterations for
-- any matrix stored in memory as in this implementation.
--
-- This function works with any Fractional: in particular you can use Rational
-- inputs to compute exact results, or Float/Double inputs to do floating point
-- arithmetic.
--
-- [@total@] The sum total of the PageRank scores in the output rank vectors.
--       This constraint is needed to ensure a unique result because any
--       multiple of the PageRank vector is also a solution.
--
-- [@beta@] The probability of following a link on a given page instead of
--       making a jump to a randomly selected page (teleporting). This is
--       usually set between 0.8 and 0.9. It must be in the range [0,1].
--
-- [@matrix@] The transition matrix where m[i][j] is the probability of
--       following a link on page j to land on page i.
--
pageRank :: (Fractional a, Ord a) => a -> a -> Matrix a -> [Matrix a]
pageRank _ beta _ | beta < (fromIntegral 0) || beta > (fromIntegral 1) =
  error "pageRank requires 0 <= beta <= 1"
pageRank _ _ m | (ncols m) /= (nrows m) =
  error "pageRank only works with a square matrix."
pageRank total beta m = iterate (nextR total beta m) r0
  where r0 = matrix (ncols m) 1 $ const (total / (fromIntegral $ ncols m))

-- Given the current rank vector, calculate the next approximation of the
-- PageRank vector for sites. The total and beta are used to apply the tax
-- that simulates teleports, or jumps by the walker to a random page with
-- probability (1 - beta) at each step. When beta < 1, this guarantees that
-- we will converge to a unique solution per 1st order Markov processes.
nextR :: (Fractional a) => a -> a -> Matrix a -> Matrix a -> Matrix a
nextR total beta m r = r' + teleports
  where r' = scaleMatrix beta $ m * r
        leak = total - (sum $ toList r')
        teleports = matrix (nrows r) (ncols r) $ const (leak / numElems)
        numElems = fromIntegral $ (nrows r) * (ncols r)
