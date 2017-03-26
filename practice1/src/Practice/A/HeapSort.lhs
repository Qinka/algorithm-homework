

\section{Heap Sort}
\verb|Practice.A.HeapSort|
\par
\verb|Practice1/src/Practice/A/HeapSort.lhs|

This part of the codes is about the heap sort.

\begin{code}
module Practice.A.HeapSort
       ( heapSort
       ) where

import Practice.A.Types
\end{code}



Firstly, we should build a heap, and than, we use the heap to sort element.

\begin{code}
heapSort :: (Eq a, Ord a) => [a] -> [a]
heapSort xs = init $ seq heap $ heapSortStep len heap
  where heap = buildMaxHeap xs
        len = length xs
        heapSortStep 0 hp = hp
        heapSortStep n hp = let hp' = exchange hp 1 n
                            in seq hp' $ last hp':(heapSortStep (n-1) $ maxHeapify (init hp') 1)
\end{code}

To sort a list:
\begin{spec}
heapSort [1,45,7,3,5,5,76,234]
\end{spec}
