
\section{Back Tracking (Problem III \& IV)}
\label{sec:bt}

\begin{code}
module Practice.D.BackTracking
       ( BackTracking(..)
       , generalBT
       , NQueens(..)
       , nQueens
       , bknap
       ) where

import Data.Ratio
import Control.Monad
import Control.Monad.Cont
import Control.Monad.State.Strict
import Data.Vector.Unboxed (Vector,Unbox,(//),(!))
import qualified Data.Vector         as V
import qualified Data.Vector.Unboxed as UV
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


\begin{code}
newtype BackTrackT s r a = BackTrackT (Cont (s -> r) a)
                           deriving (Functor,Applicative,Monad,MonadCont)
instance MonadState s (BackTrackT s r) where
  get          = BackTrackT $ cont $ \next curState -> next curState curState
  put newState = BackTrackT $ cont $ \next curState -> next () newState
\end{code}

There we define a type-class: BackTracking.

\begin{code}
class BackTracking a where
  type Stack a
  type Selects a
  type FinalSelect a
  initState :: a -> (Stack a,Selects a)
  boundCheck :: a ->Selects a -> BackTrackT (Stack a) r Bool
  isAnswer :: a -> Selects a -> BackTrackT (Stack a) r Bool
  isEnd :: a -> Selects a -> BackTrackT (Stack a) r Bool
  next :: a -> Selects a -> BackTrackT (Stack a) r (Selects a)
  back :: a -> Selects a -> BackTrackT (Stack a) r (Selects a)
  pushAnswer :: a -> Selects a -> BackTrackT (Stack a) r ()
  toFinal :: a -> Stack a -> FinalSelect a
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

generalBT :: (BackTracking a) => a -> FinalSelect a
generalBT cfg = toFinal cfg $ runCont' (stepM istate) (\_ -> id) istack
  where (istack,istate) = initState cfg
        stepM state = do
          iE <- isEnd cfg state
          if iE then return ()
            else do
            bc <- boundCheck cfg state
            if bc then do
              isAnswer cfg state >>=
                (\iA -> when iA $ pushAnswer cfg state)
              next cfg state >>= stepM
              else do
              back cfg state >>= stepM
        runCont' (BackTrackT m) = runCont m

\end{code}

\subsection{N-Queens(Problem III)}
\label{sec:bt:nq}

So I can try to write a instance, n-queens problem, of the general method of the back tracking algorithm.
We need to define the question itself, first.

\begin{code}
newtype NQueens a = NQueens {numOfQueens :: a}
\end{code}

The problem's answer is just about, or just say, effected by, the number of the queens.
Then the problem will be sovled with back tracking.

\begin{code}
instance (Unbox a,Integral a,Ord a) => BackTracking (NQueens a) where  
\end{code}

First of all, the current selecting state of this problem should be \lstinline|Vector| or the unboxed one,
and so does the final select.

\begin{code}
  type Stack       (NQueens a) =         [[a]]
  type Selects     (NQueens a) = UV.Vector a
  type FinalSelect (NQueens a) =         [[a]]
\end{code}

The initial state of the problem should be a list with a element(0).

\begin{code}
  initState _ = ([],UV.fromList [0])
\end{code}

And the bound check function of the problem.

\begin{code}
  boundCheck cfg sel = return $ place && lim
    where lim  = fromIntegral k < numOfQueens cfg
              &&             xk < numOfQueens cfg
          len = UV.length sel
          k = len - 1
          xk = UV.head sel
          xi k i = sel UV.! (k - i)
          place = and [ xi k i /= xk
                        && abs (xi k i - xk) /= fromIntegral (abs (i - k))
                      | i <- [0..k-1]]
\end{code}

After check the bound, we can check  whether a selet set is a answer.

\begin{code}
  isAnswer cfg sel = return $ fromIntegral (UV.length sel) == numOfQueens cfg
\end{code}

And then we will find out whether the halt condition is true.

\begin{code}
  isEnd _ sel = return $ UV.null sel
\end{code}

Next function is about the next selet set state of the selets.

\begin{code}
  next cfg sel = do
    iA <- isAnswer cfg sel
    if iA then return $ sel // [(0,UV.head sel + 1)]
      else return $ UV.cons 0 sel
\end{code}

When the ``back tracking'' is needed, we will call the back tracking function: \lstinline|back|.

\begin{code}
  back cfg sel = if h >= numOfQueens cfg
                 then let sel' = UV.tail sel
                      in if UV.null sel'
                         then return sel'
                         else return $ sel' //  [(0,UV.head sel' + 1)]
                 else return $ sel // [(0,UV.head sel + 1)]
    where h = UV.head sel
\end{code}


Push the answer to stack.
\begin{code}
  pushAnswer cfg i = modify (\s -> UV.toList i:s)
\end{code}

Finally, we need a function to transform the ``current state select set'' to final answer.

\begin{code}
  toFinal _ =id
\end{code}

Then the final method for wrapping is the following.

\begin{code}
nQueens :: (Unbox a,Integral a,Ord a) => a -> [[a]]
nQueens n = generalBT (NQueens n)
\end{code}

\endinput
\subsection{Binary Knapsack(Problem IV)}
\label{sec:bt:bk}

 这个真的做不出来了。。

\begin{code}
bknap :: (Ord a,Integral a) => [(Ratio a,Ratio a)] -> Ratio a -> ((Ratio a,Ratio a),[Bool]) -- value,weight
bknap item l = maximum $ filter (\((_,w),_) -> w <= l) $ map (getSum item) sels
  where sels =[x | x <- mapM (const [True,False]) [1..length item] ]
        getitem (a,b) True = (a,b)
        getitem _ False = (0,0)
        pariAdd (a,b) (c,d) = (a+c,b+d)
        getSum items sel = (foldl pariAdd (0,0) $ zipWith getitem items sel,sel)
\end{code}
