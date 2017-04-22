
% \usepackage{amsmath}


\section{Knapsack Problem (Problem I \& II)}
\label{sec:knapsack}

\begin{code}
module Practice.C.KnapsackProblem
       ( dpbMkM
       , dpbBest
       , gkpFill
       , gkpSort
       ) where

import Data.Array
import Data.Map.Lazy(Map(..))
import Data.Ratio
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Map.Lazy as MapL
\end{code}

\subsection{Introduction}
\label{sec:knapsack:intr}

The knapsack problem is a kind of optimization problem.
That says when a theft stealing, it has to decide the things which he steals,
because its knapsack can not hold all the things, and it want to steal as much costed as possible.

So the main problem of that is that find a combination of the items, each of which has value and weight, 
and such combination's sum of the values is maximum, and sum of the weight is close enough to the limit.

Then a key of this problem is the ratio of the weight and the value.
That problem is looked like that it can be tranform to a single-variable optimization problem.
Though that seems to work for \verb|fractional knapsack problem|, but we still can get an approximate result for \verb|0-1 knapsack Problem|.

\subsection{Dynamic Programming for 0-1 Knapsack Problem (Problem I)}
\label{sec:knapsack:dp01}

\subsubsection{Solve}
\label{sec:knapsack:dp01:solve}

Let $V(i,j)$ to represent sum of the value of first $i$ items, which can be stored in the knapsack.
Then we can get a function:
\begin{equation}\label{eq:funcv}
V(i,j) = \left\{
    \begin{array}{rl}
    0 & ,\, i = 0 \, \text{or} \, j = 0; \\
    V(i-1,j) & ,\,j < w_i; \\
    \max\{V(i-1,j),V(i-1,j-w_i)+v_i\} & ,\, j > w_i 
    \end{array}\right.
\end{equation}

Firstly ,we need to define a type of the weight and value.
\begin{code}
type Weight = Int
type Value  = Int
type Item = (Weight,Value)
\end{code}

Then we can write the function to instance function (\ref{eq:funcv}). But there is a difference: I will try to record the each step in $V(i,j)$.

\begin{code}
dpbMkM :: [Item] -> Weight-> Map (Int,Weight) (Value,[Int])
dpbMkM items w = m
  where len = length items
        m = Map.fromList $  [((i,j),func i j) | (i,j) <- pair]
        func 0 _ = (0,[])
        func _ 0 = (0,[])
        func i j = let (w,v) = items !! (i-1)
                   in if w > j  then m Map.! (i-1,j)
                      else let next = m Map.! (i-1,j)
                               (last,ll) = m Map.! (i-1,j-w)
                               this = (v + last, i:ll)
                           in max this next
        ws = fst <$> items
        mkWs i = map (\k -> (i,k)) $ List.nub $ filter (>= 0) $  map (w-) $  sum <$> map (zipWith (*) (drop i ws)) (mapM (const [0,1]) [1..len])
        pair = concat $ map mkWs [0..len]
\end{code}

Then I can get the best answer.

\begin{code}
dpbBest ::  Map (Int,Weight) (Value,[Int]) -> (Value,[Int])
dpbBest = snd . Map.findMax
\end{code}

\subsubsection{Example \& Test}
\label{sec:knapsack:dp01:test}

So let's test with the following example:
\begin{table}[h!]
    \centering
    \begin{tabular}{|c|c|c|}
        \hline item & weights & values \\ 
        \hline A & 50 & 200 \\ 
        \hline B & 30 & 180 \\ 
        \hline C & 45 & 225 \\ 
        \hline D & 25 & 200 \\ 
        \hline E & 5 & 50 \\ 
        \hline 
    \end{tabular} 
   \caption{Knapsack Problem Example}
   \label{tab:knapsack:eg}
\end{table}

Then the example will be transform to the Haskell codes:

\begin{spec}
example :: [(Int,Int)]
example = [(50,200),(30,180),(45,225),(25,200),(5,50)]
\end{spec}

The result of the example when the weight capacity is 100 :

\begin{spec}
rst :: Map (Int, Weight) (Value, [Int])
rst = dpbMkM example 100
\end{spec}

And the final result is:

\begin{spec}
r :: (Value,[Int])
r = dpbBest rst
\end{spec}

The out is

\begin{ghci}
ghciLL> :{
ghciLL| example :: [(Int,Int)]
ghciLL| example = [(50,200),(30,180),(45,225),(25,200),(5,50)]
ghciLL| rst :: Map (Int, Weight) (Value, [Int])
ghciLL| rst = dpbMkM example 100
ghciLL| r :: (Value,[Int])
ghciLL| r = dpbBest rst
ghciLL| :}
ghciLL> rst
fromList [((0,0),(0,[])),((0,5),(0,[])),((0,15),(0,[])),((0,20),(0,[])),((0,25),(0,[])),((0,30),(0,[])),((0,40),(0,[])),((0,45),(0,[])),((0,50),(0,[])),((0,55),(0,[])),((0,65),(0,[])),((0,70),(0,[])),((0,75),(0,[])),((0,95),(0,[])),((0,100),(0,[])),((1,0),(0,[])),((1,20),(0,[])),((1,25),(0,[])),((1,30),(0,[])),((1,40),(0,[])),((1,45),(0,[])),((1,50),(200,[1])),((1,55),(200,[1])),((1,65),(200,[1])),((1,70),(200,[1])),((1,75),(200,[1])),((1,95),(200,[1])),((1,100),(200,[1])),((2,25),(0,[])),((2,30),(180,[2])),((2,50),(200,[1])),((2,55),(200,[1])),((2,70),(200,[1])),((2,75),(200,[1])),((2,95),(380,[2,1])),((2,100),(380,[2,1])),((3,70),(225,[3])),((3,75),(405,[3,2])),((3,95),(425,[3,1])),((3,100),(425,[3,1])),((4,95),(425,[4,3])),((4,100),(605,[4,3,2])),((5,100),(605,[4,3,2]))]
ghciLL> r
(605,[4,3,2])
\end{ghci}

The output means that the ``B'', ``C'', and ``D'' was selected, of which sum is 605.


\subsection{Greedy for Fractional Knapsack Problem (Problem II)}
\label{sec:knapsack:greedy}

\subsubsection{Solve}
\label{sec:knapsack:greedy:solve}

For the fractional knapsack problem, the greedy is the best way to solve it.
Because when we solve such a problem with the strategy -- ``steal'' the most valuable first. And the difference between two knapsack problems is the former's items can not be split.
    
So firstly, we need to sort the items.

\begin{code}
type Item' = (Weight,Value,Int)
gkpSort :: [Item] -> [Item']
gkpSort = reverse . List.sortOn ratio . zipWith (\i (a,b)-> (a,b,i)) [0..]
  where ratio (w,v,_) = v % w
\end{code}

The we can ``fill'' them.

\begin{code}
gkpFill' :: [Item'] -> Ratio Value -> Weight -> [(Int,Int,Ratio Value)]
gkpFill'  [] v _  = []
gkpFill' (x@(w,v,i):xs) av aw | aw <= 0   = []
                              | otherwise = let av' = av + (v * ww % w) 
                                                ww  = min w aw
                                            in i `seq` av' `seq` (i,ww,av'):gkpFill' xs av' (aw-w)
gkpFill :: [Item] -> Weight -> (Ratio Value,[(Int,Int)])
gkpFill items w = ((\(_,_,i) -> i) $ last is,(\(a,b,c) -> (a,b)) <$> is)
  where is = gkpFill' (gkpSort items) 0 w
\end{code}

\subsubsection{Example \& Test}
\label{sec:knapsack:greedy:test}

Then we test with the example at the table \ref{tab:knapsack:eg}.

The result of the example when the weight capacity is 100 :

\begin{spec}
rst :: (Ratio Value,[(Int,Int)])
rst = gkpFill example 100
\end{spec}


The out is

\begin{ghci}
ghciLL> :{
ghciLL| example :: [(Int,Int)]
ghciLL| example = [(50,200),(30,180),(45,225),(25,200),(5,50)]
ghciLL| rst :: (Ratio Value,[(Int,Int)])
ghciLL| rst = gkpFill example 100
ghciLL| :}
ghciLL> rst
(630 % 1,[(4,5),(3,25),(1,30),(2,40)])
\end{ghci}

And the output means that ``B'', ``C'', ``D'', and ``E'' was selected,
of which sum is 630.

