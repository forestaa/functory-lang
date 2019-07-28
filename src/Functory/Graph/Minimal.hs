module Functory.Graph.Minimal where

import Control.Monad.Error.Class
import Data.Extensible
import Data.Extensible.Effect.Default
import Functory.Graph
import Functory.Syntax.Minimal
import RIO
import qualified RIO.Map as Map


type GraphWithOutput a =  (Vertex a, Vertex a -> Graph a)
type VertexMap a = Map.Map String (Vertex a)
data ConvertError = 
    UndefinedNode String 
  deriving Show
type ConvertEff a = '["vertices" >: State (VertexMap a), EitherDef ConvertError]
runConvertEff :: VertexMap a -> Eff (ConvertEff a) b -> Either ConvertError b
runConvertEff vertices = leaveEff . runEitherDef . flip (evalStateEff @"vertices") vertices

lookupVertice :: (Lookup xs "vertices" (State (VertexMap a)), MonadError ConvertError (Eff xs)) => String -> Eff xs (Vertex a)
lookupVertice x = getsEff #vertices (Map.lookup x) >>= \case
  Nothing -> throwError $ UndefinedNode x
  Just v -> pure v
callGraph :: Ord a => Term -> Eff (ConvertEff a) (GraphWithOutput a)
callGraph (Variable x) = do
  vertice <- lookupVertice x
  pure (vertice, \out -> newGraph [vertice, out] [newEdge vertice out])
callGraph (Application f x) = do
  (input, graphf) <- callGraph f
  (_, graphx) <- callGraph x
  pure (input, \out -> graphx input `union` graphf out) -- overlay
