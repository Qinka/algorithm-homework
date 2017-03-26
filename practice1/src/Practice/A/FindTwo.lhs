

\section{Find Two Element -- Problem (I)}
\verb|Practice.A.FindTwo|
\par
\verb|Practice1/src/Practice/A/FindTwo.lhs|

This part will solve the problem 1: find two elements whose sum is the given value $x$.

\begin{code}
module Practice.A.FindTwo
       ( findTwo
       ) where


import Practice.A.Types
import Practice.A.HeapSort
import Practice.A.BinFind
\end{code}


We need an algorithm in $O(n\log{n})$ to sort and an algorithm in $O(\log{n}$ to find the element. And I use heap sort and binary find to this thing.

Firstly, it should be sort. And then, for each element, find out whether there is an element that is what we want.


\begin{code}
findTwo :: (Eq a, Ord a, Num a) => [a] -> a -> Bool
findTwo xs' s = or $! func <$> sq
  where xs = heapSort xs'
        func i = binFind (drop i xs) $ findIs $ s - head (drop (i-1) xs)
        sq = seq xs [1..length xs-1] :: [Int]
\end{code}
