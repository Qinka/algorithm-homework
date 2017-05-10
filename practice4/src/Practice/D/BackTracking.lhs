
\section{Back Tracking (Problem III \& IV)}
\label{sec:bt}

\begin{code}
module Practice.D.BackTracking
       ( BackTracking(..)
       , generalBT
       , NQueens(..)
       , nQueens
       , BinKnap(..)
       , BKStack(..)
       , evaluatedV
       , binKnap
       ) where

import Data.List(sortOn)
import Data.Ratio
import Control.Monad
import Control.Monad.Cont
import Control.Monad.State.Strict
import Data.Vector.Unboxed (Vector,Unbox,(//),(!),cons)
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
  boundCheck :: a -> Selects a -> BackTrackT (Stack a) r Bool
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


\begin{code}
data BinKnap a = BinKnap { bkItems   :: UV.Vector (a,a)
                         , bkMaxWeg  :: a
                         }
                 deriving(Eq,Show)
\end{code}

\begin{code}
data BKStack a = BKStack { answerCandidate :: UV.Vector Bool
                         , finalValue      :: a
                         , finalWeight     :: a
                         }
                 deriving(Eq,Show)
\end{code}

\begin{code}
evaluatedV :: (Unbox a,Fractional a,Ord a) => a -> UV.Vector (a,a) -> UV.Vector Bool -> (a,a,a)
evaluatedV maxWeightLimit items' sels = (totalW,preW,preV)
  where len = UV.length sels
        iLen = UV.length items
        items = UV.reverse items'
        (itemS,itemC) = UV.splitAt (iLen - len) items
        doSel (tW,tV) (cW,cV,True ) = (cW + tW,cV + tV)
        doSel t       (_ ,_ ,False) = t
        (totalW,totalV) = UV.foldl' doSel (0,0) $ UV.zipWith (\(w,v) c -> (w,v,c)) itemS sels
        doPrd mWL (tW,tV) (iW,iV) = if iW + tW <= mWL
                                    then (tW+iW,tV+iV)
                                    else let per = 1 - (tW - mWL) / iW
                                         in (tW + iW * per,tV + iV * per)
        (preW,preV) = UV.foldl' (doPrd maxWeightLimit) (totalW,totalV) itemC
\end{code}

\begin{code}
instance (Unbox a,Fractional a,Ord a) => BackTracking (BinKnap a) where
\end{code}

\begin{code}
  type FinalSelect (BinKnap a) = ([Bool],[(a,a)])
  type Selects     (BinKnap a) = UV.Vector Bool
  type Stack       (BinKnap a) = BKStack a
\end{code}

\begin{code}
  initState cfg = ( BKStack{ answerCandidate = UV.fromList []
                           , finalValue      = 0
                           , finalWeight     = 0
                           }
                  , iS
                  )
    where initStat mWL (cW,cV,cS) (iW,iV) = if iW + cW <= mWL
                                            then (iW + cW,iV + cV,True `cons` cS)
                                            else (cW,cV,False `cons` cS)
          (iW,iV,iS) = UV.foldl' (initStat (bkMaxWeg cfg)) (0,0,UV.fromList []) $ bkItems cfg
\end{code}

\begin{code}
  boundCheck cfg sel = do
    let (_,_,eV) = evaluatedV (bkMaxWeg cfg) (bkItems cfg) sel
    BKStack _ fV _ <- get
    return $ eV > fV 
\end{code}

\begin{code}
  isAnswer cfg sels = do
    return $ UV.length (bkItems cfg) == UV.length sels
\end{code}
\begin{code}
  isEnd _ sels = return $ UV.null sels
\end{code}
\begin{code}
  next cfg sels =
    if UV.length (bkItems cfg) == UV.length sels
    then return $ rollbackNext sels 
    else return $ True `cons` sels
    where rollbackNext sl =
            if UV.null sl then  sl
            else let h = UV.head sl
                 in if h then sl // [(0,False)]
                    else rollbackNext $ UV.tail sl
\end{code}
\begin{code}
  back cfg sels = rollbackNext sels
    where rollbackNext sl =
            if UV.null sl then return sl
            else let h = UV.head sl
                 in if h then return $ sl // [(0,False)]
                    else rollbackNext $ UV.tail sl
  pushAnswer cfg sels = do
    let (fW,_,fV) = evaluatedV (bkMaxWeg cfg) (bkItems cfg) sels
    put $ BKStack sels fV fW
  toFinal cfg (BKStack sl _ _) = (UV.toList $ UV.reverse sl,UV.toList $ bkItems cfg)
\end{code}


\begin{code}
binKnap :: (Unbox a,Fractional a,Ord a) => a -> [(a,a)] -> ([Bool],[(a,a)])
binKnap maxWeight items = generalBT $ BinKnap (UV.fromList $ sortOn (\(a,b) -> b / a) items) maxWeight
\end{code}
