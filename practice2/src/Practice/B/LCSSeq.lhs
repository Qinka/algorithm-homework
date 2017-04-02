

% Require -XMultiWayIf

\section{Longest Common Subsequence (Problem II}
\label{sec:lcs}


\begin{code}
module Practice.B.LCSSeq
       ( lcsMkM
       , Direction(..)
       , mkLCSSolution
       , lcs
       ) where
\end{code}


In this section, the longest common subsequence problem will be solved.

Before this problem solved via dynamic programming, the optimal substructure should be defined.
Let $$X=<x_1, x_2, \dots, x_m$$, and $$Y=<y_1, y_2, \dots, y_n>$$ to be two sequences,
and let $$Z=<z_1, z_2, \dots, z_k>$$ to be the longest subsequence for $X$ and $Y$.

And we can say that: when $x_m == y_n$ is true and $z_k=x_m=y_n$ is true, too, we can say that $Z_{k-1}$ is a subsequence of $X_{m-1}$ and $Y_{n-1}$;
when $x_m \neq y_n$ and $z_k \neq x_m$, we can say that $Z$ is a subsequence of $X_{m-1}$ and $Y_{n}$;
when $x_m \neq y_n$ and $z_k \neq y_n$, we can say that $Z$ is a subsequence of $X_{m}$ and $Y_{n-1}$;

So if we define a table $c[i,j]$, which will be used to the length of the longest subsequence of $X_i$ and $Y_j$.
Then we can get an expression:
\begin{equation*}
  c\left[i,j\right] = \left\{
    \begin{aligned}
      0 & i = 0 \text{or} j = 0, \\
      c\left[i-1,j-1\right] +1 & i,j > 0 \text{and} x_i = y_j, \\
      max\left(c\left[i,j-1\right],c\left[i-1,\right]\right) & i,j > 0 \text{and} x_i \neq y_j.
    \end{aligned}\right.
\end{equation*}



So then I can write a function to get the table of a tuple where will hold the length and how the ``search'' move.

Before writing the function, we should define the action of the ``search''.
There are three direction are need : ``West'', ``North-West'', and ``North'', which mean the $\leftarrow$, $\nwarrow$, and $\rightarrow$, in order.
And the ``direction'' \verb|end| is also necessary, which means ``search'' is ended.

\begin{code}
data Direction = West | NorthWest | North | End
               deriving (Show,Eq)
\end{code}

Then we can write the function:
\begin{code}
lcsMkM :: Eq a => [a] -> [a] -> Int -> Int -> (Int, Direction)
lcsMkM xs ys = mkM
  where mkM i j = if | i < 0 || j < 0 -> (0,End)
                     | xs !! i == ys !! j -> (nwItem + 1, NorthWest)
                     | northItem >= westItem -> (northItem, North)
                     | otherwise -> (westItem, West)
          where westItem  = fst $ lcsMkM xs ys i (j-1)
                northItem = fst $ lcsMkM xs ys (i-1) j
                nwItem    = fst $ lcsMkM xs ys (i-1) (j-1)
\end{code}



Then I can use the table which was evaluate be \lstinline|lcsMkM|.

\begin{code}
mkLCSSolution :: Eq a =>  (Int -> Int -> Direction) -> [a] -> Int -> Int -> [a]
mkLCSSolution func xs i j = reverse $ mkLCSSolution' func xs i j
mkLCSSolution' :: Eq a =>  (Int -> Int -> Direction) -> [a] -> Int -> Int -> [a]
mkLCSSolution' func xs i j | i <0 || j < 0 = []
                          | func i j == NorthWest = xs !! i : mkLCSSolution' func xs (i-1) (j-1)
                          | func i j == North = mkLCSSolution' func xs (i-1) j
                          | otherwise = mkLCSSolution' func xs i $ j - 1
\end{code}

For easilier to be used, I defined
\begin{code}
lcs :: Eq a => [a] -> [a] -> Int -> Int -> Direction
lcs xs ys i j = snd $ lcsMkM xs ys i j
\end{code}
