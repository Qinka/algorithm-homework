

\begin{code}
module Practice.A.HeapSort
       ( heapSort
       ) where

import Practice.A.Types
\end{code}

HeapSort
\begin{code}
heapSort :: (Eq a, Ord a) => [a] -> [a]
heapSort xs = init $ seq heap $ heapSortStep len heap
  where heap = buildMaxHeap xs
        len = length xs
        heapSortStep 0 hp = hp
        heapSortStep n hp = let hp' = exchange hp 1 n
                            in seq hp' $ last hp':(heapSortStep (n-1) $ maxHeapify (init hp') 1)
\end{code}
