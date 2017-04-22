
\section{Bin Packing Problem (Problem IV)}
\label{sec:bpp}

\begin{code}
module Practice.C.BinPackingProblem
       ( bppSolve
       ) where

import Data.Function
import Data.Ratio
\end{code}


\subsection{Introduction}
\label{sec:bpp:intr}

The \verb|bin packing problem| is about packing objects of different volumes into a finite number of bins of containers in a way that the number of the bins used is minimum.
To be honest, such a problem is the combinatorial NP-hard problem. So it should be solve with approximate algorithm.

\subsection{Solve}
\label{sec:bpp:solve}

There the first-fit algorithm, and it is a very straightforward greedy approximation algorithm, according to Wikipedia\footnote{from: \url{https://en.wikipedia.org/wiki/Bin\_packing\_problem\#First-fit\_algorithm}}.

The advantages of the first-fit algorithm are:
\begin{itemize}
    \item input datas can be arbitrary order
    \item $\Theta(n\lg n)$
\end{itemize}

\paragraph{General Discription}

The first-fit will hold a bin at the beginning, and the bin will be packed.
When bin is full or can not be packed into, it will start a new bin.
When packing, it will find the pack-able.

\paragraph{Using in Haskell}
The first fit is not easy to be implemented in Haskell, so there I will using fixed-point function.

Firstly we need to initiate the datas.

\begin{code}
bppInit :: Integral a => [Ratio a] -> [(Ratio a,[Int])]
bppInit vs = zipWith with vs [1..]
  where with v i = (v,[i])
\end{code}

Next we can write the fixed-point function.

\begin{code}
bppFix :: Integral a => [(Ratio a,[Int])] -> [(Ratio a,[Int])]
bppFix [] = []
bppFix is = let next@(n:ns) = bppMerge is
            in if length next == length is
               then n:bppFix ns
               else bppFix $ bppMerge is
\end{code}

In in function, a function named \lstinline[language=Haskell]{bppMerge} is need. That function will merge two containers whose sum will be smaller than 1.

\begin{code}
bppMergeStep :: Integral a => (Ratio a,[Int]) -> [(Ratio a,[Int])] -> [(Ratio a,[Int])] ->  [(Ratio a,[Int])]
bppMergeStep i as [] = i:as
bppMergeStep (i,il) ls ((r,rl):rs) = if i + r <= 1 then (i+r,il++rl):ls ++ rs
                                     else bppMergeStep (i,il) ((r,rl):ls) rs
bppMerge :: Integral a => [(Ratio a,[Int])] -> [(Ratio a,[Int])]
bppMerge [] = []
bppMerge (i:is) = bppMergeStep i [] is
\end{code}

Then the final function to solve.

\begin{code}
bppSolve :: Integral a => [Ratio a] -> [(Ratio a,[Int])]
bppSolve = bppFix . bppInit
\end{code}

\subsection{Example \& Test}

Here we will use the example:
$$
<0.5,0.7,0.3,0.9,0.6,0.8,0.1,0.4,0.2,0.5>
$$

\begin{spec}
example :: [Ratio Int]
example = [0.5,0.7,0.3,0.9,0.6,0.8,0.1,0.4,0.2,0.5]
\end{spec}

And then test them:

\begin{spec}
rst :: [(Ratio Int,[Int])]
rst = bppSolve example
\end{spec}

And then, the output is

\begin{ghci}
ghciLL> :{
ghciLL| example :: [Ratio Int]
ghciLL| example = [0.5,0.7,0.3,0.9,0.6,0.8,0.1,0.4,0.2,0.5]
ghciLL| rst :: [(Ratio Int,[Int])]
ghciLL| rst = bppSolve example
ghciLL| :}
ghciLL> rst
[(9 % 10,[1,3,7]),(7 % 10,[10,9]),(4 % 5,[6]),(1 % 1,[8,5]),(7 % 10,[2]),(9 % 10,[4])]
ghciLL> length rst
6
\end{ghci}

That means we use 6 bins to hold all items, and each of bin holds:
$0.9$, $0.7$, $0.8$, $1$, $0.7$, $0.9$.