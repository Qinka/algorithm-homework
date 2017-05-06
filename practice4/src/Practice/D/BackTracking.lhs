
\section{Back Tracking (Problem III \& IV)}
\label{sec:bt}

\begin{code}
module Practice.D.BackTracking
       (
       ) where
\end{code}

\subsection{Introduction}
\label{sec:bt:intro}

The ``back tracking'' is a kind of the algorithm to solve the problem such as n-queen problem(problem iii) and
binary knapsack problem(problem iv).

The basic idea of the back tracking is similar with violent traverse, but it can efficiently get rif of the ``error'' answer.
The back tracking is searching on a state space tree, and with the 
explicit constraints and implicit constraints, the algorithm will find out whether a node in the tree is an active node, a current node, or a dead node.

\subsection{General Method}
\label{sec:bt:gm}

So we can generally define a set of functions and data structs to represent the \textit{\textbf{back tracking}} algorithm.

There we define a type-class: BackTracking.

\begin{code}
class BackTracking a where
  type Selects a
  type FinalSelect a
  initState :: a -> Selects a
  boundCheck :: a -> Selects a -> Bool
  isAnswer :: a -> Selects a -> Bool
  isEnd :: a -> Selects a -> Bool
  next :: a -> Selects a -> Selects a
  back :: a -> Selects a -> Selects a
  toFinal :: a -> Selects a -> FinalSelect a
\end{code}

Then type\footnote{with type family} \lstinline|Selects a| is the answer of the current state.
The ``variable'' \lstinline|initState| is the initial state of the answer. 
The method \lstinline|boundCheck| will check the bound of the answer.
The method \lstinline|isAnswer| will check the current status is an answer or not.
The method \lstinline|isEnd| will check whether the state is at the bottom of the tree.
The method \lstinline|next| will generate the next state of the answer.
The method \lstinline|back| will back the former state when failed.
The method \lstinline|toFinal| will transform the current states to final select

Next is the general function to solve the back tracking.

\begin{code}
generalBT :: (BackTracking a,FinalSelect a ~ fs) => a -> [fs]
generalBT cfg = step [] istate
  where istate = initState cfg
        step stack state | boundCheck cfg state =
                            let stack' = if isAnswer cfg state
                                         then toFinal cfg state:stack
                                         else stack
                            in step stack' $ next cfg state
                         | isEnd cfg state = stack 
                         | otherwise = step stack $ back state
\end{code}
