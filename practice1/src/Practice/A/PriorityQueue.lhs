
\begin{code}
module Practice.A.PriorityQueue
       ( heapMaximum
       , heapExtractMax
       , heapIncreaseKey
       , heapInsert
       ) where

import Practice.A.Types
\end{code}

\begin{code}
heapMaximum :: Heap a -> a
heapMaximum = head
\end{code}

\begin{code}
heapExtractMax :: Ord a =>  Heap a -> (a,Heap a)
heapExtractMax [] = error "heap underflow"
heapExtractMax s@(x:xs) = (x,flip maxHeapify 1 . init . exchange s 1 $ length s)
\end{code}

\begin{code}
heapIncreaseKey :: Ord a => Heap a -> Int -> a -> Heap a
heapIncreaseKey heap i key = if key < heap <!! i
                             then error "new key is smaller then current key"
                             else f i $! change heap i key
  where f 1 h = h
        f n h = let p = heapParent n
                  in if h <!! p < h <!! n
                     then f p $ exchange h n p
                     else h
\end{code}

\begin{code}
heapInsert :: Ord a => Heap a -> a -> Heap a
heapInsert heap a = let len = length heap + 1
                    in heapIncreaseKey (heap++[a]) len a
\end{code}
