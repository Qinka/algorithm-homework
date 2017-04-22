


\section{Simple Scheduling Problem (Problem III)}
\label{sec:scheduling}

\begin{code}
module Practice.C.Scheduling
       ( scheduling
       ) where
import Data.Ratio
import qualified Data.List as List
\end{code}


\subsection{Restatement}
\label{sec:scheduling:restatement}

This problem is a simple problem of scheduling. A number of jobs, $j_1,j_2,\dots,j_n$,
are given, with the running time, $t_1,t_2,\dots,t_n$.
And what we need to do is planning a scheduling in order to minimize the average completion time,
with the assumption -- non-preemption.

The definition of the completion time is the time begun when arriving and ended when completed.
And an additional assumption is the all jobs arrived at the same time.

\subsection{Solve}
\label{sec:scheduling:solve}

We just using the greedy strategy to solve this problem, and get the minimum average completion time.
The completion include the waiting time and executing time. The executing times are given.
For job $j_i$, its waiting time is the completion time of job, $j_{i-1}$, however first job needn't wait.

So the total completion time's expression is looked like:
\begin{equation}\label{eq:sch:origin}
  j_1 + (j_1 + j_2) + ((j_1 + j_2)+j_3) + \dots + ((\dots((j_1 + j_2) + j_3)\dots)+j_n)
\end{equation}


The expression (\ref{eq:sch:origin}) is quite a mass, so there is a simple one:
\begin{equation}
 n*j_1 + (n-1)*j_2 + \dots + 1 * j_n
\end{equation}

To be honest, there is a more simple one:

\begin{equation}\label{eq:sch:sum}
\sum\limits_{i=1}^{n}(n-i+1) \times j_i
\end{equation}

And the average is
\begin{equation}\label{eq:sch:final}
\frac{1}{n}\sum\limits_{i=1}^{n}(n-i+1) \times j_i
\end{equation}


So we can adjust the order of the jobs to get the minimum completion time. 
From the expression (\ref{eq:sch:sum}), we can know that to get the minimum time,
the shortest jobs should be done firstly.

Then the program can be written.

Firstly, there should be a function which can tag on the jobs. It is like sparse matrix.

\begin{code}
noteJobs :: Integral a => [Ratio a] -> [(Ratio a,Int)]
noteJobs is = zip is [1..]
\end{code}

Then we need to sort the jobs by time.

\begin{code}
sortJobsByTime :: Integral a => [(Ratio a,Int)] -> [(Ratio a,Int)]
sortJobsByTime = List.sort
\end{code}

We can get final scheduling in the form of the jobs' indexes.

\begin{code}
finalScheduling :: [(Ratio a,Int)] -> [Int]
finalScheduling = map snd
\end{code}

And then we can evalute the average time finally.

\begin{code}
finalAvgTime :: Integral a => [(Ratio a,Int)]  -> Ratio a
finalAvgTime is = total / List.genericLength is
  where total  = sum $ zipWith (*) [1..] $ reverse $ fst <$> is         
\end{code}

Finally, the result will be:

\begin{code}
scheduling :: Integral a => [Ratio a] -> (Ratio a,[Int])
scheduling is' = (finalAvgTime is,finalScheduling is)
  where is = sortJobsByTime $ noteJobs is'
\end{code}

\subsection{Example \& Test}
\label{sec:scheduling:test}

The let's test with the following example.

$$<j_1,j_2,j_3,j_4>:<15,8,3,10>$$

The input and output is

\begin{ghci}
ghciLL> scheduling [15,8,3,10]
(71 % 4,[3,2,4,1])
ghciLL> 71 / 4
17.75
\end{ghci}

That means the scheduling should be $j_3$, $j_2$, $j_4$, and then $j_1$, and that will cost 17.75%
\footnote{\lstinline[language=haskell]{71 \% 4} is a kind of notion in Haskell to represent fraction, which is same with $\frac{71}{4}$}.