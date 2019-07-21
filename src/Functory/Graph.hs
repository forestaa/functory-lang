module Functory.Graph where

import Data.Extensible
import RIO
import qualified RIO.Set as Set

newtype Vertice a = Vertice a deriving (Show, Eq, Ord) via a
newtype Edge a = Edge (Record '["source" >: Vertice a, "target" >: Vertice a]) deriving (Show, Eq, Ord) via (Record '["source" >: Vertice a, "target" >: Vertice a])
newtype Graph a = Graph (Record '["vertices" >: Set.Set (Vertice a), "edges" >: Set.Set (Edge a)]) deriving (Show, Eq) via (Record '["vertices" >: Set.Set (Vertice a), "edges" >: Set.Set (Edge a)])

newEdge :: Vertice a -> Vertice a -> Edge a
newEdge src tgt = Edge $ #source @= src <: #target @= tgt <: nil
newGraph :: Ord a => [Vertice a] -> [Edge a] -> Graph a
newGraph vertices edges = Graph $ #vertices @= Set.fromList vertices <: #edges @= Set.fromList edges <: nil
addVerticesToGraph :: Ord a => [Vertice a] -> Graph a -> Graph a
addVerticesToGraph vertices (Graph r) = Graph $ over #vertices (flip (foldr Set.insert) vertices) r
addEdgesToGraph :: Ord a => [Edge a] -> Graph a -> Graph a
addEdgesToGraph edges (Graph r) = Graph $ over #edges (flip (foldr Set.insert) edges) r
addVerticesAndEdgesToGraph :: Ord a => [Vertice a] -> [Edge a] -> Graph a -> Graph a
addVerticesAndEdgesToGraph vertices edges = addEdgesToGraph edges . addVerticesToGraph vertices
union :: Ord a => Graph a -> Graph a -> Graph a
union (Graph r) (Graph r') = Graph $ #vertices @= ((r ^. #vertices) `Set.union` (r' ^. #vertices)) <: #edges @= ((r ^. #edges) `Set.union` (r' ^. #edges)) <: nil

