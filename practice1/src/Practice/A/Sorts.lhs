
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


\begin{enumerate}
\label{req:1}
\item The algorithm runs in O(n) time.
\label{req:2}
\item The algorithm is stable.
\label{req:3}
\item The algorithm sorts in place, using no more than a constant amount of storage space in addition to the original array.
\end{enumerate}

0 => False
1 => True

Give an algorithm that satisfies criteria \ref{req:1} and \ref{req:2} above.

Give an algorithm that satisfies criteria \ref{req:1} and \ref{req:3} above.

Give an algorithm that satisfies criteria \ref{req:2} and \ref{req:3} above.


\begin{code}
countSortWith :: (Bounded a, Enum a, Ord a) => Int -> Int -> [(a,b)] -> [(a,b)]
countSortWith ma' mi' xs = installItem mi xs $! addCount xs $! replicate (length xs) 0
  where rt = addCount xs $ replicate (ma - mi + 1) 0
        ma = max ma' mi'
        mi = min mi' ma'
        --addCount :: Enum a => 
        addCount [] x = map (\ia -> sum $ take ia x) [1..length x]
        addCount ((a,b):as) xs = addCount as $! modify xs (fromEnum a) (+1)


installItem :: (Enum a,Ord a) => Int -> [(a,b)] -> [Int] -> [(a,b)]
installItem base x@(i:xs) is = iiS x is $! replicate (length x) i
  where iiS [] _ rt = rt
        iiS (x@(a,b):xs) is rt = let rt' = change rt (is !! (fromEnum a - base)) x
                                     is' = modify is (fromEnum a - base + 1) (\x -> x -1)
                                 in seq is' $ iiS xs is' $! rt'
countSort :: [Bool] -> [Bool]
countSort xs = replicate f False ++ replicate t True
  where (f,t) = addCount xs (0,0) -- False True
        addCount [] x = x
        addCount (False:as) (x,y) = let x' = x + 1
                                    in seq x' $ addCount as (x',y)
        addCount (True :as) (x,y) = let y' = y + 1
                                    in seq y' $ addCount as (x,y')
\end{code}

\begin{code}
moveSort :: [Bool] -> [Bool]
moveSort bs = moveSortStep (0,length bs -1) bs
  where moveSortStep (a,b) xs =
          if a >= b
          then xs
          else case (xs !! a,xs !! b) of
            (True,False) -> moveSortStep (a+1,b) $ exchange xs (a+1) (1+b)
            (False,False) -> moveSortStep (a+1,b) xs
            _ -> moveSortStep (a,b-1) xs
                                   
\end{code}

\begin{code}
filterSort :: [Bool] -> [Bool]
filterSort xs = filter (not) xs ++ filter (id) xs
\end{code}

\begin{code}
filterSortWith :: [(Bool,a)] -> [(Bool,a)]
filterSortWith xs = filter (not.fst) xs ++ filter (id.fst) xs
\end{code}

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

\begin{code}
\end{code}


Can you use any of your sorting algorithms from parts (a)–(c) as the sorting method used in line 2 of RADIX-SORT, so that RADIX-SORT sorts n records with b-bit keys in O(bn) time? Explain how or why not.



Suppose that the n records have keys in the range from 1 to k. Show how to modify counting sort so that it sorts the records in place in O(n+k) time. You may use O(k) storage outside the input array. Is your algorithm stable? (Hint: How would you do it for k = 3?)
