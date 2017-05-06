
\section{All Pairs Shortest Paths with FAPSP (Problem II)}
\label{sec:fapsp}

\begin{code}
module Practice.D.FastAllPairsShortestPaths
       ( smmult
       , fapsp
       , inf
       ) where

import Data.Array.Accelerate hiding(Num,Fractional,replicate,ceiling,fromIntegral)
import qualified Data.Array.Accelerate as A

inf :: Fractional a => a
inf = 1 / 0
\end{code}

When we need to find out the shortest paths of the pairs, we need some algorithms to run it out.
Here I choose ``Fast All Pairs Shortest Path'' as the algorithm to solve the problem.

\subsection{Square Matrix Multiply}
\label{sec:fapsp:smm}

In the \textbf{\textit{Fast All Pairs Shortest Path}} algorithm, the matrix is used to represent the graph.
And an operation, named ``Square Matrix Multipl'' will be used to compute the shortest path.

\begin{code}
smmult :: (A.Num e,A.Ord e,A.Fractional e) => Acc (Array DIM2 e) -> Acc (Array DIM2 e) -> Acc (Array DIM2 e)
smmult arr brr
  = A.fold A.min inf
  $ A.zipWith (+) arrRepl brrRepl
  where
    Z :. rowsA :. _     = unlift (A.shape arr)    :: Z :. Exp Int :. Exp Int
    Z :. _     :. colsB = unlift (A.shape brr)    :: Z :. Exp Int :. Exp Int
    --
    arrRepl             = A.replicate (lift $ Z :. All   :. colsB :. All) arr
    brrRepl             = A.replicate (lift $ Z :. rowsA :. All   :. All) (A.transpose brr)
\end{code}


To get the shortest path, we need to do the multiply many times.
If our matrix of the graph is $W_{m \times m}$, the first level of the target $L^{(1)}$ is equal to $W$.
Then the next level target is $L^{(2)} = W^2$. The recursion formula for level $n$ is $L^{(2^n} = L^{n} \cdot L^{n}$.
And the target of the final level is $L^{n-1}$, or just say $\lceil \lg{(n-1)} \rceil$. And we can provide that $\lceil 2^{\lg{(n-1)}} \rceil \geq n -1$.

So then I can define the function.

\begin{code}
fapsp :: (A.Num e,A.Ord e,A.Fractional e) => (Acc (Array DIM2 e) -> Array DIM2 e) -> Array DIM2 e -> Array DIM2 e
fapsp run g = run $ acc (use g) funcs
  where acc = foldr (\func gh -> func gh gh)
        funcs = replicate rn smmult
        Z :. n :. _  = arrayShape g :: Z :. Int :. Int
        rn = ceiling $ logBase 2 $ fromIntegral n - 1
\end{code}


\subsection{Example \& Test}
\label{sec:fapsp:test}

I test those with the graph in the.
\begin{figure}[h!]
  \centering
  \begin{tikzpicture}[node distance=3cm,>=stealth',bend angle=20,auto]
    \tikzstyle{point}[circle,think,draw-blue!75,fill=blue!20,minimum size=6mm]
    \begin{scope}
      \node[point](a){1};
      \node[point](b){2};
    \end{scope}
  \end{tikzpicture}
  \caption{Example Graph}
  \label{fig:fapsp:graph}
\end{figure}
