
\section{Basic Types and Function}
\verb|Practice.A.Types|
\par
\verb|Practice1/src/Practice/A/Types.lhs|

This part is for the basic function and types.

\begin{code}
module Practice.A.Types
       ( Heap(..) -- For heap
       , heapParent
       , heapLeft, heapRight
       , maxHeapify
       , buildMaxHeap
       , (<!!)
       , exchange
       , change
       , modify
       ) where

import GHC.Exts
\end{code}

\subsection{Heap}

In this part, there are some types and some functions for maintainning and building a heap.


\paragraph{data Heap}
Heap is a kind of data struct, and a kind of tree. We use array to present a heap.

\begin{code}
type Heap a = [a]
\end{code}

\subsubsection{Baisc Operations}

I define the basic operations for heap:


\paragraph{PARENT}
To get the parent node.
\begin{code}
heapParent :: Int -> Int
heapParent = round . (/2) . fromIntegral
\end{code}

If we want to find the parent node index:
\begin{spec}
heapParent thisIndex
\end{spec}

If we want to find the parent node element:
\begin{spec}
heap !! heapParent thisIndex
\end{spec}

\item[LEFT]  To get the left-sub node.
\begin{code}
heapLeft :: Int -> Int
heapLeft = (*2)
\end{code}

\item[RIGHT] To get the right-sub node.
\begin{code}
heapRight :: Int -> Int
heapRight = (+1) . (*2)
\end{code}


\paragraph{exchange}

To exchange two elements in the heap, I defined the function:

\begin{code}
exchange :: Heap a -> Int -> Int -> Heap a
exchange heap a' b' = x ++ bb:y ++ aa:z
  where (a,b) = (`mod` length heap) `minMaxW` (a'-1,b'-1)
        aa = heap !! a
        bb = heap !! b
        (x,ox)  = a `splitAt` heap
        (y',z') = (b-a) `splitAt` ox
        y = tailS y'
        z = tailS z'
        tailS [] = []
        tailS xs = tail xs
        seq' x = seq' x
        minMaxW f (x,y) = let xx = f x; yy = f y
                          in if x < y
                             then (seq' xx, seq' yy)
                             else (seq' yy, seq' xx)
                    
\end{code}

E.G.:
\begin{spec}
exchange heap elementIndexA elementIndexB
\end{spec}

\paragraph{modify}

To modify an element in heap and get the new one, I defined the function:

\begin{code}
modify :: Heap a -> Int -> (a -> a) -> Heap a
modify heap i f = let (l,r) = splitAt i heap
                  in init l ++ f (last l) : r
\end{code}

E.G.:\\
If you want to auto-increment a element:
\begin{spec}
modify heap index (\x -> x + 1)
\end{spec}
                   
\paragraph{change} 

To change an element to another, I defined the function: 
   
\begin{code}
change :: Heap a -> Int -> a -> Heap a
change heap i item = modify heap i (\_ -> item)
\end{code}

\subsubsection{Maintain A Heap}

To maintain a max heap, I write this function to keep a max heap.
\begin{code}
maxHeapify :: (Eq a, Ord a) => Heap a -> Int -> Heap a
maxHeapify heap i = if largest /= i
                    then maxHeapify (exchange heap i largest) largest
                    else heap
  where l = heapLeft i
        r = heapRight i
        len = length heap
        largest = let lag = if l <= len && heap <!! l > heap <!! i
                              then l else i
                  in if r <= len && heap <!! r > heap <!! lag
                     then r else lag
\end{code}

Then I use function \lstinline|maxHeapify| to build a max heap:

\begin{code}
buildMaxHeap :: (Eq a, Ord a) => Heap a -> Heap a
buildMaxHeap heap = buildMaxHeapStep (heapParent $ length heap) heap
  where buildMaxHeapStep 1 h  = maxHeapify h 1
        buildMaxHeapStep n hp = seq n $! buildMaxHeapStep (n-1) $! maxHeapify hp n
\end{code}

\subsubsection{Others}

Here, a operator \lstinline|<!!| is defined to handle the difference between the initial values of the Haskell and pseudocode.

\begin{code}
(<!!) :: [a] -> Int -> a
a <!! i = (!!) a $! (i-1)
\end{code}

