
\section{Shortest Path in Multistage Graphs (Problem IV)}
\label{sec:spimg}

\begin{code}
module Practice.B.ShortestPath
       ( spmgMkM
       ) where
\end{code}

To solve the this problem, I try to use \textit{dynamic programming} to solve this problem.

Firstly, I have to define a table for get the shortest path from node $i$ to node $j$.
Then, if the solution is the shortest, then it's substructure's solution is the best.

I write this function to get the table, and this function \textbf{DO NOT CHECK THE INPUT of THE GRAPHS}.

%% left  -> to
%% right -> from

\begin{code}
spmgMkM :: [(Int,Int,Int)] -> Int -> Int -> (Int,[Int])
spmgMkM gs = mkM
  where mkM i j | i == j = (0,[])
                | otherwise = filterG gs i j $ 
                  let sub = (\(_,f,_) -> f) <$> filter (\(t,_,_) -> t == i) gs
                      subrt k = let (l1,s1)   = spmgMkM gs k j
                                    (l2,s2:_) = spmgMkM gs i k
                                in (l1+l2,s2:s1)
                      rts = subrt <$> sub
                  in minimum rts
        filterG gs a b func = case filter (\(x,y,_) -> x == a && y == b) gs of
          [] -> func
          (p,_,q):_ -> (q,[p])
\end{code}

\subsection{Example \& Test}
\label{sec:lcsseqtest}

Here I will test this followings codes.


\paragraph{Example 1}
\begin{spec}
gs :: [(Int,Int,Int)]
gs = [(2,1,9),(3,1,7),(4,1,3),(5,1,2),(6,2,4),(7,2,2),(8,2,1),(6,3,2),(7,3,7),(8,4,11),(7,5,11),(8,5,8),(9,6,6),(10,6,5),(9,7,4),(10,7,3),(10,8,5),(11,8,6),(12,9,4),(12,10,2),(12,11,5)]
spmgMkM gs   12 1
\end{spec}
And  then the return is \lstinline|(16,[12,10,6,3])|, that means the shortest path is
$1 \rightarrow 3 \rightarrow 6 \rightarrow 10 \rightarrow 12$, and that will cost 16.
