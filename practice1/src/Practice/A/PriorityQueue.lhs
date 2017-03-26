
\section{Priority Queue -- Problem (II)}
\verb|Practice.A.PriorityQueue|
\par
\verb|Practice1/src/Practice/A/PriorityQueue.lhs|

\begin{code}
module Practice.A.PriorityQueue
       ( heapMaximum
       , heapExtractMax
       , heapIncreaseKey
       , heapInsert
       ) where

import Practice.A.Types
\end{code}

\subsection{Basic Operations}

Firstly, I defined some basic function to operate heap which will be used for priority queue.

First function is to get the max-one of the heap.

\begin{code}
heapMaximum :: Heap a -> a
heapMaximum = head
\end{code}

The second of function is pop the max of the heap and get the new one.

\begin{code}
heapExtractMax :: Ord a =>  Heap a -> (a,Heap a)
heapExtractMax [] = error "heap underflow"
heapExtractMax s@(x:xs) = (x,flip maxHeapify 1 . init . exchange s 1 $ length s)
\end{code}

\subsection{Main Operations}

To increase a element's priority. Change the element's value and heapify.

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

To increase a element to the heap. 

\begin{code}
heapInsert :: Ord a => Heap a -> a -> Heap a
heapInsert heap a = let len = length heap + 1
                    in heapIncreaseKey (heap++[a]) len a
\end{code}
