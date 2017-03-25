
\section{Basic Types and Function}

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

In this part, there a some type and some functions for maintainning and building a heap

To define the types for Heap
\begin{code}
type Heap a = [a]
\end{code}

to get the parent node
\begin{code}
heapParent :: Int -> Int
heapParent = round . (/2) . fromIntegral
\end{code}

to get the left and right node
\begin{code}
heapLeft :: Int -> Int
heapLeft = (*2)
heapRight :: Int -> Int
heapRight = (+1) . (*2)
\end{code}

exchange 2 element
\begin{code}
exchange :: Heap a -> Int -> Int -> Heap a
exchange heap a' b' = let a'' = (a'-1) `mod` length heap
                          b'' = (b'-1) `mod` length heap
                          a = min a'' b''
                          b = max a'' b''
                          aa = heap !! a
                          bb = heap !! b
                          (x,ox)  = a `splitAt` heap
                          (y',z') = (b-a) `splitAt` ox
                          y = tailS y'
                          z = tailS z'
                      in x ++ bb:y ++ aa:z
  where tailS [] = []
        tailS xs = tail xs
\end{code}

\begin{code}
modify :: Heap a -> Int -> (a -> a) -> Heap a
modify heap i f = let (l,r) = splitAt i heap
                  in init l ++ f (last l) : r

change :: Heap a -> Int -> a -> Heap a
change heap i item = modify heap i (\_ -> item)
\end{code}


to maintain a max heap
\begin{code}
maxHeapify :: (Eq a, Ord a) => Heap a -> Int -> Heap a
maxHeapify heap i = if largest /= i
                    then maxHeapify (exchange heap i largest) largest
                    else heap
  where l = heapLeft i
        r = heapRight i
        len = length heap
        largest = let lag = if l <= len && heap <!! l > heap <!! i then l else i
                  in if r <= len && heap <!! r > heap <!! lag then r else lag
\end{code}
to build a max heap
\begin{code}
buildMaxHeap :: (Eq a, Ord a) => Heap a -> Heap a
buildMaxHeap heap = buildMaxHeapStep (heapParent $ length heap) heap
  where buildMaxHeapStep 1 h    = maxHeapify h 1
        buildMaxHeapStep n hp = seq n $! buildMaxHeapStep (n-1) $! maxHeapify hp n
\end{code}


\begin{code}
(<!!) :: [a] -> Int -> a
a <!! i = seq (i-1) $ a !! (i-1)
\end{code}
