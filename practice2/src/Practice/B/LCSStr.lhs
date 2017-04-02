

\section{Longest Common Substring (Problem V)}
\label{sec:lcsstr}

\begin{code}
module Practice.B.LCSStr
       ( lcs
       , lcsMkM
       , mkLCSSolution
       ) where

\end{code}

In this section, I will go with \verb|Problem V|. It is same with longest common substring.
Firstly, let $$X=<x_1, x_2, \dots, x_m$$, and $$Y=<y_1, y_2, \dots, y_n>$$ to be two sequences,
and let $$Z=<z_1, z_2, \dots, z_k>$$ to be the longest substring for $X$ and $Y$.

The best substructure of this problem is that when $x_m = y_n$, let $z_k = x_m = y_n$, and $Z_{k-1}$ will be the longest common substring of
$X_{m-1}$ and $Y_{n-1}$, and that when $x_m \neq y_n$, and $z_k$ should be $null$.
Then we can get an expression:
\begin{equation*}
  c\left[i,j\right] = \left\{
    \begin{aligned}
      0 & i = 0 \text{or} j = 0, \\
      c\left[i-1,j-1\right] +1 & i,j > 0 \text{and} x_i = y_j, \\
      0 & i,j > 0 \text{and} x_i \neq y_j.
    \end{aligned}\right.
\end{equation*}

\begin{code}
lcsMkM :: Eq a => [a] -> [a] -> Int -> Int -> (Int,Bool)
lcsMkM xs ys = mkM
  where mkM i j = if xs !! i == ys !! j
                  then let nextItem = fst $ lcsMkM xs ys (i-1) (j-1)
                       in(nextItem + 1, True)
                  else (0,False)
\end{code}

\begin{code}
mkLCSSolution :: Eq a => [a] -> [a] -> Int -> Int -> [a]
mkLCSSolution xs ys i j  = reverse $ mkLCSSolution' (lcs xs ys) xs bi bj
  where (_,_,bi,bj) = maximum $ (\(i,j) -> let (a,b) = lcsMkM xs ys i j in(a,b,i,j)) <$> [(p,q) | p <- [0..i],q <- [0..j]]
mkLCSSolution' :: Eq a => (Int -> Int -> Bool) -> [a] -> Int -> Int -> [a]
mkLCSSolution' func xs i j | i < 0 || j < 0 = []
                           | func i j = xs !! i : mkLCSSolution' func xs (i-1) (j-1)
                           | otherwise = []
\end{code}

\begin{code}
lcs :: Eq a => [a] -> [a] -> Int -> Int -> Bool
lcs xs ys a b = snd $ lcsMkM xs ys a b
\end{code}
