
\section{Quick Sort -- Problem (III)}
\verb|Practice.A.Quicksort|
\par
\verb|src/Practice/A/Quicksort.lhs|


\begin{code}
module Practice.A.Quicksort
       ( split
       , quickSort
       , quickSortR
       ) where

import System.IO.Unsafe
import Control.Monad.Random.Class
\end{code}

\subsection{Common part -- REUSE}

For the normal quick sort and random quick sort, they have a common part for sort.
The difference between the normal one and random one is the guard's index(or called location).

So the params for the \lstinline|split| are a location and the function called for recursion.
Then is will "output" a function for quick sort

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

\subsection{Quick Sort}

\paragraph{Quick Sort}

For the normal one, set the guard's location is $1$.

\begin{code}
quickSort :: Ord a => [a] -> [a]
quickSort = split 1 quickSort
\end{code}

\paragraph{Random Quick Sork}

For the random quick sort, the function \lstinline|unsafePerformIO| is needed to change a 
side-effect-with random number to the pure one. 

\begin{code}
quickSortR :: Ord a => [a] -> [a]
quickSortR xs = split n quickSortR xs
  where n = unsafePerformIO $ getRandomR (0,length xs - 1)
\end{code}
