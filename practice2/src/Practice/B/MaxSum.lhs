

\section{Max Sum (Problem III)}
\label{sec:maxsum}

\begin{code}
module Practice.B.MaxSum
       ( dpMaxSum
       ) where
\end{code}

This section is about using \textit{dynamic programming} to solve the ``Max Sum'' problem.

This problem is about that yor are given $n$ integers(there may be negative ones but not all) $a_1,a_2,\dots,a_n$,
determine $i$ adn $j$ which maximize the sum from $a_i$ to $a_j$.

\begin{code}
dpMaxSum :: (Num a,Ord a) => [a] -> [a]
dpMaxSum = dpMaxSumStep 0 0 []
dpMaxSumStep :: (Num a,Ord a) => a -> a -> [a] -> [a] -> [a]
dpMaxSumStep _   _ buf [] = buf
dpMaxSumStep sum b buf (x:xs) | b > 0 = let b' = b + x
                                            sum' = if b' > sum then b' else sum
                                            buf' = if b' > sum then x:buf else buf
                                        in buf `seq` dpMaxSumStep sum' b buf' xs
                              | otherwise =  dpMaxSumStep sum x [x] xs
\end{code}
