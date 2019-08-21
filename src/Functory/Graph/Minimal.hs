module Functory.Graph.Minimal where

import Data.Extensible
import Functory.Graph
import Functory.Syntax.Minimal
import RIO
import qualified RIO.Map as Map
import SString


type GraphWithOutput a =  (Vertex a, Vertex a -> Graph a)
type VertexMap a = Map.Map SString (Vertex a)
data ConvertError = 
    UndefinedNode SString 
  deriving (Show, Eq)
runConvertEff :: VertexMap a 
  -> Eff '[
      "vertices" >: State (VertexMap a)
    , "callGraphMinimal" >: EitherEff ConvertError
    ] b 
  -> Either ConvertError b
runConvertEff vertices = leaveEff . runEitherEff @"callGraphMinimal" . flip (evalStateEff @"vertices") vertices

lookupVertice :: (
    Lookup xs "vertices" (State (VertexMap a))
  , Lookup xs "callGraphMinimal" (EitherEff ConvertError)
  ) => SString -> Eff xs (Vertex a)
lookupVertice x = getsEff #vertices (Map.lookup x) >>= \case
  Just v -> pure v
  Nothing -> throwEff #callGraphMinimal $ UndefinedNode x

callGraph :: (
    Lookup xs "vertices" (State (VertexMap a))
  , Lookup xs "callGraphMinimal" (EitherEff ConvertError)
  , Ord a) 
  => Term 
  -> Eff xs (GraphWithOutput a)
callGraph (Variable x) = do
  vertice <- lookupVertice x
  pure (vertice, \out -> newGraph [vertice, out] [newEdge vertice out])
callGraph (Application f x) = do
  (input, graphf) <- callGraph f
  (_, graphx) <- callGraph x
  pure (input, \out -> graphx input `union` graphf out) -- overlay
