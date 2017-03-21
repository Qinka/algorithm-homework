
\begin{code}
module Practice.A.Quicksort
       ( split
       , quickSort
       , quickSortR
       ) where

import System.IO.Unsafe
import Control.Monad.Random.Class

\end{code}

\begin{code}
split :: (Ord a) => Int -> ([a] -> [a]) -> [a] -> [a]
split _ []  _ = []
split _ [x] _ = [x]
split i' xs f = let l = filter (<  guard) xs
                    r = filter (>= guard) xs
                 in f l ++ f r
  where i = if | i' < 0 = 0
               |  otherwise = i' `mod` length xs
        guard = xs !! i
\end{code}

quick sort
\begin{code}
quickSort :: [a] -> [a]
quickSort = split 1 quickSort
\end{code}

random quick sork
\begin{code}
quickSortR :: [a] -> [a]
qucikSortR xs = split n quickSortR
  where n = unsafePerformIO $ getRandomR (0,length xs - 1)
\end{code}
