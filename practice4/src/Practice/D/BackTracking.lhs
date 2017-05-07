
\section{Back Tracking (Problem III \& IV)}
\label{sec:bt}

\begin{code}
module Practice.D.BackTracking
       ( BackTracking(..)
       , generalBT
       , NQueens(..)
       , nQueens
       ) where

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
  pushAnswer :: a -> FinalSelect a -> BackTrackT (Stack a) r ()
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

generalBT :: (BackTracking a) => a -> Stack a
generalBT cfg = runCont' (stepM istate) (\_ -> id) istack
  where (istack,istate) = initState cfg
        stepM state = do
          iE <- isEnd cfg state
          if iE then return ()
            else do
            bc <- boundCheck cfg state
            if bc then do
              isAnswer cfg state >>=
                (\iA -> when iA $ pushAnswer cfg (toFinal cfg state))
              next cfg state >>= stepM
              else do
              back cfg state >>= stepM
        runCont' (BackTrackT m) = runCont m

\end{code}
\begin{spec}
generalBT :: (BackTracking a,FinalSelect a ~ fs) => a -> [fs]
generalBT cfg = step istack istate
  where (istack,istate) = initState cfg
        step stack state | isEnd cfg stack state = stack 
                         | boundCheck cfg state =
                             let stack' = if isAnswer cfg state
                                          then pushAnswer cfg (toFinal cfg state) stack
                                          else stack
                             in step stack' $ next cfg state
                         | otherwise = step stack $ back cfg state
\end{spec}

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
  type Stack       (NQueens a) = [[a]]
  type Selects     (NQueens a) = UV.Vector a
  type FinalSelect (NQueens a) =          [a]
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
          place = and [ xi k i /= xk && abs (xi k i - xk)
                        /= fromIntegral (abs (i - k))
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
  pushAnswer _ i = modify (\s -> i:s)
\end{code}

Finally, we need a function to transform the ``current state select set'' to final answer.

\begin{code}
  toFinal _ sel = UV.toList sel 
\end{code}

Then the final method for wrapping is the following.

\begin{code}
nQueens :: (Unbox a,Integral a,Ord a) => a -> [[a]]
nQueens n = generalBT (NQueens n)
\end{code}

\endinput
\subsection{Binary Knapsack(Problem IV)}
\label{sec:bt:bk}

Another instance of the back tracking is  binary knapsack problem.
The elements of the problem includes the informations of the knapsack, and the limit of the weight.

\begin{code}
data BinKnap a = BinKnap { itemsOfBK :: UV.Vector (a,a)
                         , totalOfBK :: (a,a) -- weight,value
                         , maxOfBK   :: a
                         }
\end{code}

The stack of the state in the searching.

\begin{code}
data BKStack a = BKStack { bkFP ::  a
                         , bkCP ::  a
                         , bkFW ::  a
                         , bkCW ::  a
                         , bkX  :: [a]
                         }
\end{code}


And a three-state type is needed to define the state of a knapsack.

\begin{code}
type KnapState = (Bool,Bool) -- (Up/Down,Alive/Dead) False/True
\end{code}

The next to be written is instance.

\begin{code}
instance (Fractional a,Ord a,Unbox a) => BackTracking (BinKnap a) where
\end{code}

The ``current select set'' of this problem should be a list of the bit, and the final result of the problem should be the list of the ones selected.
\begin{code}
  type Stack       (BinKnap a) = BKStack a
  type Selects     (BinKnap a) = UV.Vector KnapState
  type FinalSelect (BinKnap a) = [a]
\end{code}


Then the initial state of the this problem is the list with one pari of \lstinline|True|.
\begin{code}
  initState _ = (BKStack 0 0 [(False,False)],UV.fromList [(False,False)])
\end{code}

The bound check function of the problem.
\begin{code}
  boundCheck cfg sel = return limit
    where bound m c b rs | UV.null rs = 0
                         | otherwise =
                           let r = UV.head rs
                           in if fst r + c < m
                              then bound m (c + fst r) (b + snd r)
                                   $ UV.tail rs
                              else b + (1 - (c-m)/fst r) * snd r
          limit = UV.length sel <= UV.length (itemsOfBK cfg)
               && cw + fst h <= m
            where cw = fst $ curOfBK cfg
                  h = itemsOfBK cfg ! (UV.length sel - 1)
                                
\end{code}

After check the bound, we can check whether a select set is a answers.

\begin{code}
  isAnswer cfg sel = 
\end{code}

\begin{code}
  isEnd cfg sel = return $ UV.length sel == UV.length (itemsOfBK cfg)
               && UV.and (UV.map sel)
\end{code}
