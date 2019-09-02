module Functory.Graph where

import Data.Aeson

import Control.Monad.Reader
import Data.Extensible
import Data.Extensible.Effect.Default
import RIO
import qualified RIO.Map as Map
import qualified RIO.Map.Partial as Map
import qualified RIO.Set as Set



newtype Vertex a = Vertex a deriving (Show, Eq, Ord, ToJSON) via a
newtype Edge a = Edge (Record '["item" >: a, "source" >: Vertex a, "target" >: Vertex a]) deriving (Show, Eq, Ord) via (Record '["source" >: Vertex a, "target" >: Vertex a])
newtype Graph a = Graph (Record '["vertices" >: Set.Set (Vertex a), "edges" >: Set.Set (Edge a)]) deriving (Show, Eq) via (Record '["vertices" >: Set.Set (Vertex a), "edges" >: Set.Set (Edge a)])

newEdge :: a -> Vertex a -> Vertex a -> Edge a
newEdge a src tgt = Edge $ #item @= a <: #source @= src <: #target @= tgt <: nil
newGraph :: Ord a => [Vertex a] -> [Edge a] -> Graph a
newGraph vertices edges = Graph $ #vertices @= Set.fromList vertices <: #edges @= Set.fromList edges <: nil
addVerticesToGraph :: Ord a => [Vertex a] -> Graph a -> Graph a
addVerticesToGraph vertices (Graph r) = Graph $ over #vertices (flip (foldr Set.insert) vertices) r
addEdgesToGraph :: Ord a => [Edge a] -> Graph a -> Graph a
addEdgesToGraph edges (Graph r) = Graph $ over #edges (flip (foldr Set.insert) edges) r
addVerticesAndEdgesToGraph :: Ord a => [Vertex a] -> [Edge a] -> Graph a -> Graph a
addVerticesAndEdgesToGraph vertices edges = addEdgesToGraph edges . addVerticesToGraph vertices
union :: Ord a => Graph a -> Graph a -> Graph a
union (Graph r) (Graph r') = Graph $ #vertices @= ((r ^. #vertices) `Set.union` (r' ^. #vertices)) <: #edges @= ((r ^. #edges) `Set.union` (r' ^. #edges)) <: nil



newtype VisualizedGraph a = VisualizedGraph (Record '["vertex" >: Vertex a, "children" >: [VisualizedGraph a]]) deriving (Eq, Show, ToJSON) via (Record '["vertex" >: Vertex a, "children" >: [VisualizedGraph a]])

visualize :: (Ord a, Show a) => Vertex a -> Graph a -> VisualizedGraph a
visualize output (Graph graph) = leaveEff . (`runReaderDef` edgesMap) $ visualizeInternal output
  where
    edgesMap = Set.foldr (\(Edge e) -> Map.adjust ((:) (e ^. #source)) (e ^. #target)) (Map.fromList . fmap (, []) . Set.toList $ graph ^. #vertices) (graph ^. #edges)

visualizeInternal :: (Ord a, Show a) => Vertex a -> Eff '[ReaderDef (Map.Map (Vertex a) [Vertex a])] (VisualizedGraph a)
visualizeInternal v = do
  children <- asks $ flip (Map.!) v
  visualizedChildren <- traverse visualizeInternal children
  pure . VisualizedGraph $ #vertex @= v <: #children @= visualizedChildren <: nil
