
% \usepackage{amsmath}

\section{Matrix-Chain Product (Problem I)}
\label{sec:mcp}

\begin{code}
module Practice.B.MatrixChainProduct
       ( rmvMkM
       , rmv
       , mkMCPSolution
       ) where       
\end{code}

In this section, I will solve the problem (I) -- matrix-chain product problem.
This problem is care about: how to schedule the order of the matrix-chain product's evaluating.

For two normal matrix:
\[A =
  \begin{bmatrix}
    a_{11} & \dots & a_{1s} \\
    \vdots & \ddots & \vdots \\
    a_{m1} & \dots & a_{ms}
  \end{bmatrix}_{m \times s}

B = 
  \begin{bmatrix}
    a_{11} & \dots & a_{1n} \\
    \vdots & \ddots & \vdots \\
    a_{s1} & \dots & a_{sn}
  \end{bmatrix}_{s \times n}
\]

If we need to get product of the $A$ and $B$. Generally, that is the $O(m \times n \times s)$ algorithm.
We can reduce the times of product by find a best order to evaluate the expressions.

\subsection{The Way to Solve}
\label{sec:thewaytosolve}

The ways to solve this problem are the followings which I prefer to use:
\begin{itemize}
\item Dynamic Programming
\item Genetic Alogrithm
\end{itemize}

The \textit{Dynamic Programming} is the algorithm which can get the exact answer, but not the all.
And the \textit{Genetic Alogrithm} will get an approximate answer, but not the all, either.

The main method to be used here, is \verb|Dynamic Programming|.

\subsection{Dynamic Programming}
\label{sec:dp}

For matrixs $A_1,A_2,A_3,\dots,A_n$,
I solve the this thing with a bottom-up direction, trying to evaluating each kind of combinations.
For example, I need to get the best order for product $A_i,\dots,A_j$,
I should to find the best order for product $A_i,A_{i+1}$,$\dots$, $A_{i+p},\dots,\A_{j-q}$,$\dots$, and $\A_{j-1},A_j$.
So we need to structure a two tables to hold the cost and ``order'', and use memoization.

\subsection{Genetic Alogrithm}
\label{sec:ga}

For matrixs, when the genetic alogrithm is needed, there should be three elements:
\begin{itemize}
\item encode the matrixs to gene
\item the rule to evaluate the fitness of the gene
\item cross
\end{itemize}

When running such alogrithm, we can get an approximate answer.


\subsection{Solution}
\label{sec:solution}

To solve this problem, firstly, I need a function which can get the table which hold cost and order.

\subsubsection{Recursion And Memoization Version}
\label{sec:ram}

Here is the version of the function which is using recursion and memoization.
To be honest, this might be an up-down way for get the tabels.

\begin{code}
rmvMkM :: [Int] -> Int -> Int -> (Int,Int)
rmvMkM xs = mkM
  where mkM i j = if i == j
                  then (0,0)
                  else let i' = min i j
                           j' = max i j
                           ks = [i' .. j'-1]
                       in minimum $ loop i' j' <$> ks
        loop i j k = let q = l + r +  _i * _kk * _jj
                         l = fst $ rmvMkM xs i k
                         r = fst $ rmvMkM xs (k+1) j
                         _i  = xs !! i
                         _kk = xs !! (k + 1)
                         _jj = xs !! (j + 1)
                     in q `seq` (q,k)
\end{code}

To be used with \lstinline[language=Haskell]|mkSolution|, there we define a function:
\begin{code}
rmv :: [Int] -> Int -> Int -> Int
rmv xs i j = snd $ rmvMkM xs i j
\end{code}


\subsubsection{Construct Solution}
\label{sec:constructsolution}


When we can get the tables, we can get the answer.

\begin{code}
mkMCPSolution :: (Int -> Int -> Int) -> Int -> Int ->String
mkMCPSolution func i j | i == j = "A_{" ++ show i ++ "}"
                    | otherwise = let i' = min i j
                                      j' = max i j
                                  in    "\\left("
                                     ++ mkMCPSolution func i' (func i' j')
                                     ++ mkMCPSolution func (func i' j' + 1) j'
                                     ++ "\\right)"
\end{code}
