% \usepackage{pgf}
% \usepackage{tikz}

\section{Bellman-Ford Algorithm (Problem I)}
\label{sec:bfa}

\begin{code}
module Practice.D.BellmanFord
       ( Point
       , PointW
       , PointE
       , Points
       , EdgeW
       , Edges
       , Graph
       , bellmanFord
       ) where
import Data.Map (Map(..))
import qualified Data.Map        as Map
import qualified Data.Map.Lazy   as MapL
import qualified Data.Map.Strict as MapS

import Data.Vector (Vector(..))
import Data.Vector.Unboxed(Unbox(..))
import qualified Data.Vector         as Vector
import qualified Data.Vector.Unboxed as UVector
\end{code}

\subsection{Introduction}
\label{sec:bfa:intro}

The Bellman-Ford algorithm can solve and find out the shortest path of a normal graph.
This algorithm uses ``relax'' operation, and the weight of the graph can be an negatve.

\paragraph{Relaxation}

Each note has some points. For edge $<u,v>$, if the points of $u$ is 5, the points of the $v$ is 9 and the weight of the edge is 2,
the result of the ralexation is that the points of the $v$ should be 7, but if the points of the $v$ is 6, the result of the ralexation should not be changed.

Then the relaxation function be defined.
But let's define the \textbf{graph} first.

\begin{code}
type Point    = Int
type PointW a = a
type PointE a = (PointW a,Int)
type Points a = UVector.Vector (PointE a)
type EdgeW  a = a
type Edges  a = MapS.Map (Point,Point) a
type Graph  a = (Points a,Edges a)
inf :: Fractional a => a
inf = 1 / 0
\end{code}

Meanwhile, the function \lstinline|relax| should be like this:

\begin{code}
relax :: (Ord a,Unbox a, Num (PointW a)) => Points a -> (Point,Point) -> EdgeW a -> Points a
relax ps (u,v) w = let (u',_) = ps UVector.! u
                       (v',_) = ps UVector.! v
                   in ps UVector.// (if v' > u' + w then [(v,(u' + w,u))] else [])
\end{code}

The following function is for checking.

\begin{code}
relaxCheck :: (Ord a,Unbox a,Num (PointW a)) => Points a -> Bool -> (Point,Point) -> EdgeW a -> Bool
relaxCheck ps is (u,v) w = let (u',_) = ps UVector.! u
                               (v',_) = ps UVector.! v
                           in is && v' <= u' + w
\end{code}

\subsection{Instance of Algorithm}
\label{sec:bla:ioa}

When the relaxation function was defiend, the fuction of Bellman-Ford algorithm can be defined.

\begin{code}
bellmanFord :: (Fractional a,Ord a,Unbox a) => Graph a -> Point -> Maybe (Graph a)
bellmanFord (ps,es) i = let ps' = loop (UVector.length ps-1) es $! initPoints ps i
                        in if onceCheck ps' es
                           then Just (ps',es)
                           else Nothing
  where initPoints ps s = ps UVector.// [(k,(if k == s then 0 else inf,k)) | k <- [0..UVector.length ps-1]]
        once ps es = MapS.foldlWithKey' relax ps es
        loop 0 _ ps = ps
        loop n es ps = loop (n-1) es $! once ps es
        onceCheck ps es = MapS.foldlWithKey' (relaxCheck ps) True es
\end{code}




\subsection{Example \& Text}
\label{sec:bfa:text}

Then we can test the Bellman-Ford algorithm, with the follwing graph in figure \ref{fig:bla:g}.
\begin{figure}[h!]
\centering
\begin{tikzpicture}[node distance=3cm,>=stealth',bend angle=20,auto]
\tikzstyle{point}=[circle,thick,draw=blue!75,fill=blue!20,minimum size=6mm]
\begin{scope}
\node[point](s){s};
\node[point](t)[right of=s,above of=s]{t}
    edge [pre]             node {6}  (s);
\node[point](x)[right of=t]{x}
    edge [pre ,bend left]  node {5}  (t)
    edge [post,bend right] node {-2} (t);
\node[point](y)[below of=t]{y}
    edge [pre]             node {7}  (s)
    edge [pre]             node {8}  (t)
    edge [post,bend right] node {-3} (x);
\node[point](z)[right of=y]{z}
    edge [pre,bend left]   node {-4} (t)
    edge [pre]             node {9}  (y)
    edge [post,bend left]  node {2}  (s)
    edge [post]            node {7}  (x);
\end{scope}
\end{tikzpicture}
\caption{Graph}
\label{fig:bla:g}
\end{figure}

Transform the graph to haskell codes.

\begin{spec}
graph :: Graph Double
graph = (ps,es)
  where ps = UVector.fromList [(0,0),(0,0),(0,0),(0,0),(0,0)]
        es = MapS.fromList [((0,1),6),((0,3),7),((1,2),5),((1,3),8),((1,4),-4),((2,1),-2),((3,2),-3),((3,4),9),((4,0),2),((4,2),7)]
\end{spec}

And the result of the algorithm is 

\begin{spec}
Just ([(0.0,0),(2.0,2),(4.0,3),(7.0,0),(-2.0,1)],fromList [((0,1),6.0),((0,3),7.0),((1,2),5.0),((1,3),8.0),((1,4),-4.0),((2,1),-2.0),((3,2),-3.0),((3,4),9.0),((4,0),2.0),((4,2),7.0)])
\end{spec}

That means the graph in the figure \ref{fig:bla:r}. And the red line means the shortest path.

\begin{figure}[h!]
\centering
\begin{tikzpicture}[node distance=3cm,>=stealth',bend angle=20,auto]
\tikzstyle{point}=[circle,thick,draw=blue!75,fill=blue!20,minimum size=6mm]
\tikzstyle{biz}=[draw=red!75]
\begin{scope}
\node[point](s){s};
\node[point](t)[right of=s,above of=s]{t}
    edge [pre]             node {6}  (s);
\node[point](x)[right of=t]{x}
    edge [pre ,bend left]  node {5}  (t)
    edge [biz,post,bend right] node {-2} (t);
\node[point](y)[below of=t]{y}
    edge [biz,pre]             node {7}  (s)
    edge [pre]             node {8}  (t)
    edge [biz,post,bend right] node {-3} (x);
\node[point](z)[right of=y]{z}
    edge [biz,pre,bend left]   node {-4} (t)
    edge [pre]             node {9}  (y)
    edge [post,bend left]  node {2}  (s)
    edge [post]            node {7}  (x);
\end{scope}
\end{tikzpicture}
\caption{The Result of the Bellman-Ford Algorithm}
\label{fig:bla:r}
\end{figure}
