module Functory.Graph where

import Data.Extensible
import RIO
import qualified RIO.Set as Set



newtype Vertex b = Vertex b deriving (Show, Eq, Ord) via b
newtype Edge a b = Edge (Record '["item" >: a, "source" >: Vertex b, "target" >: Vertex b]) deriving (Show, Eq, Ord) via (Record '["item" >: a, "source" >: Vertex b, "target" >: Vertex b])
newtype Graph a b = Graph (Record '["vertices" >: Set.Set (Vertex b), "edges" >: Set.Set (Edge a b)]) deriving (Show, Eq) via (Record '["vertices" >: Set.Set (Vertex b), "edges" >: Set.Set (Edge a b)])

newEdge :: a -> Vertex b -> Vertex b -> Edge a b
newEdge a src tgt = Edge $ #item @= a <: #source @= src <: #target @= tgt <: nil
newGraph :: (Ord a, Ord b) => [Vertex b] -> [Edge a b] -> Graph a b
newGraph vertices edges = Graph $ #vertices @= Set.fromList vertices <: #edges @= Set.fromList edges <: nil
addVerticesToGraph :: Ord b => [Vertex b] -> Graph a b -> Graph a b
addVerticesToGraph vertices (Graph r) = Graph $ over #vertices (flip (foldr Set.insert) vertices) r
addEdgesToGraph :: (Ord a, Ord b) => [Edge a b] -> Graph a b -> Graph a b
addEdgesToGraph edges (Graph r) = Graph $ over #edges (flip (foldr Set.insert) edges) r
addVerticesAndEdgesToGraph :: (Ord a, Ord b) => [Vertex b] -> [Edge a b] -> Graph a b -> Graph a b
addVerticesAndEdgesToGraph vertices edges = addEdgesToGraph edges . addVerticesToGraph vertices
union :: (Ord a, Ord b) => Graph a b -> Graph a b -> Graph a b
union (Graph r) (Graph r') = Graph $ #vertices @= ((r ^. #vertices) `Set.union` (r' ^. #vertices)) <: #edges @= ((r ^. #edges) `Set.union` (r' ^. #edges)) <: nil
