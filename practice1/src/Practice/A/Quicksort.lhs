
\begin{code}
module Practice.A.Quicksort
       (
       ) where

\end{code}

\begin{code}
split :: [a] -> Int -> ([a] -> [a]) -> [a]
split xs i' f = let (l,r) = splitAt i xs
                 in f l ++ f r
  where i = if | i' < 0 = 0
               |  otherwise = i' `mod` length xs
\end{code}

quicksort
\begin{code}
quickSort :: [a] -> [a]
\end{code}
