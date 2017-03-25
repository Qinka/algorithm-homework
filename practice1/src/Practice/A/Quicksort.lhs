
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
split _ _ []  = []
split _ _ [x] = [x]
split i' f xs = let l = filter (<  guard) xs'
                    r = filter (>= guard) xs'
                 in f l ++ guard : f r
  where i = if  i' < 0 then  0 else i' `mod` length xs
        guard = xs !! i
        xs' = let (l,r) = splitAt i xs
              in l ++ tail r
\end{code}

quick sort
\begin{code}
quickSort :: Ord a => [a] -> [a]
quickSort = split 1 quickSort
\end{code}

random quick sork
\begin{code}
quickSortR :: Ord a => [a] -> [a]
quickSortR xs = split n quickSortR xs
  where n = unsafePerformIO $ getRandomR (0,length xs - 1)
\end{code}
