
\section{Binary Find}
\verb|Practice.A.BinFind|
\par
\verb|Practice1/src/Practice/A/BinFind.lhs|


\begin{code}
module Practice.A.BinFind
       ( binFind
       , findIs
       ) where

\end{code}

The binary finding to get element we need in $O(n)$.

\begin{code}
binFind :: [a] -> (a -> Ordering) -> Bool
binFind [] _ = False
binFind xs f = seq xs $ case r' of
  r:[] -> case f r of
    EQ -> True
    GT -> binFind l' f
    LT -> False
  r:rs -> case f r of
    EQ -> True
    GT -> binFind l' f
    LT -> binFind r' f
  where (l',r') = splitAt (length xs `div` 2) xs
\end{code}



\paragraph{Comparing Function Generator}

To generate a comparing function, which defined the rules of 

\begin{itemize}
\item Greater than
\item Equal to
\item Less than
\end{itemize}

\begin{code}
findIs :: (Eq a,Ord a) => a -> a -> Ordering
findIs a b
  | a > b  = GT
  | a == b = EQ
  | a < b  = LT
\end{code}
