
\section{Sort Problem -- Problem (IV)}
\verb|Practice.A.Sorts|
\par
\verb|Practice1/src/Practice/A/Sorts.lhs|

\begin{code}
module Practice.A.Sorts
       ( countSort
       , countSortWith
       , moveSort
       , filterSort
       , filterSortWith
       , sortBoolsBy
       , sortBools
       ) where

import Practice.A.Types
\end{code}

The requirements for the sort algorithm:
\begin{enumerate}
\label{req:1}
\item The algorithm runs in O(n) time.
\label{req:2}
\item The algorithm is stable.
\label{req:3}
\item The algorithm sorts in place, using no more than a constant amount of storage space in addition to the original array.
\end{enumerate}

I use: \\
0 => False \\
1 => True

Give an algorithm that satisfies criteria \ref{req:1} and \ref{req:2}.

Give an algorithm that satisfies criteria \ref{req:1} and \ref{req:3}.

Give an algorithm that satisfies criteria \ref{req:2} and \ref{req:3}.

\subsection{Count Sort}

We use count sort for a kind of $O(n)$ sort algorithm.

The function \lstinline|countSortWith| can sort a array of a pair of a key and a data.

\begin{code}
countSortWith :: (Bounded a, Enum a, Ord a) => Int -> Int -> [(a,b)] -> [(a,b)]
countSortWith ma' mi' xs = installItem mi xs $! addCount xs $! replicate (length xs) 0
  where rt = addCount xs $ replicate (ma - mi + 1) 0
        ma = max ma' mi'
        mi = min mi' ma'
        addCount [] x = map (\ia -> sum $ take ia x) [1..length x]
        addCount ((a,b):as) xs = addCount as $! modify xs (fromEnum a) (+1)
\end{code}

The function to set up

\begin{code}
installItem :: (Enum a,Ord a) => Int -> [(a,b)] -> [Int] -> [(a,b)]
installItem base x@(i:xs) is = iiS x is $! replicate (length x) i
  where iiS [] _ rt = rt
        iiS (x@(a,b):xs) is rt = 
          let rt' = change rt (is !! (fromEnum a - base)) x
              is' = modify is (fromEnum a - base + 1) (\x -> x -1)
          in seq is' $ iiS xs is' $! rt'
\end{code}

The normal count sort.

\begin{code}
countSort :: [Bool] -> [Bool]
countSort = (fst <$>) . countSortWith 1 0 . map (\x -> (x,x))
\end{code}

\subsection{``Move'' Sort}

"Move Sort" is use to sort a array whose values only include $0$ and $1$.
When sorting such array, just exchange the two element when need.
\verb|False| means $0$, and \verb|True| means $1$.

\begin{code}
moveSort :: [Bool] -> [Bool]
moveSort bs = moveSortStep (0,length bs -1) bs
  where moveSortStep (a,b) xs =
          if a >= b
          then xs
          else case (xs !! a,xs !! b) of
            (True,False)  -> moveSortStep (a+1,b) $ exchange xs (a+1) (1+b)
            (False,False) -> moveSortStep (a+1,b)   xs
            _ -> moveSortStep (a,b-1) xs              
\end{code}

\subsection{Filter Sort}

For the array whose values just have $0$ and $1$. We can just use filter or traversal way to find the $0$, and then find the $1$. It is an stable and $O(n)$ algorithm.

\begin{code}
filterSort :: [Bool] -> [Bool]
filterSort xs = filter (not) xs ++ filter (id) xs
\end{code}

\begin{code}
filterSortWith :: [(Bool,a)] -> [(Bool,a)]
filterSortWith xs = filter (not.fst) xs ++ filter (id.fst) xs
\end{code}

\subsection{Radix Sort}
We just use "filter sort" as the each step of the sort. And we use a array of Bool to present 
a binary string. 

Function \lstinline|sortBoolsBy| is the one sort by the given bit.

\begin{code}
sortBoolsBy :: Int -> [[Bool]] -> [[Bool]]
sortBoolsBy a xs = map snd . filterSortWith $ mk a <$> xs
  where mk i bs = (bs !! i,bs)
\end{code}

\begin{code}
sortBools :: Int -> [[Bool]] -> [[Bool]]
sortBools 0 xs = xs
sortBools n xs = sortBools (n-1) $! sortBoolsBy n xs
\end{code}

For sort the binary $0101$ and $0011$, we can do:
\begin{spec}
sortBools 3 [[False,True,False,True],[False,False,True,True]]
\end{spec}


