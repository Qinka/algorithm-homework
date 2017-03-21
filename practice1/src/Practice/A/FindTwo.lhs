
\begin{code}
module Practice.A.FindTwo
       ( findTwo
       ) where


import Practice.A.Types
import Practice.A.HeapSort
import Practice.A.BinFind
\end{code}

for find it
\begin{code}
findTwo :: (Eq a, Ord a, Num a) => [a] -> a -> Bool
findTwo xs' s = or $! func <$> sq
  where xs = heapSort xs'
        func i = binFind (drop i xs) $ findIs $ s - head (drop (i-1) xs)
        sq = seq xs [1..length xs-1] :: [Int]
\end{code}
