module Functory.Graph.Minimal where

import Control.Monad.Error.Class
import Data.Extensible
import Data.Extensible.Effect.Default
import qualified Functory.Graph as G
import Functory.Syntax.Minimal
import RIO
import qualified RIO.Map as Map

type GraphWithOutput a =  (G.Vertice a, G.Vertice a -> G.Graph a)
type VerticeMap a = Map.Map String (G.Vertice a)
data ConvertError = 
    UndefinedNode String 
  deriving Show
type ConvertEff a = '["vertices" >: State (VerticeMap a), EitherDef ConvertError]
runConvertEff :: VerticeMap a -> Eff (ConvertEff a) b -> Either ConvertError b
runConvertEff vertices = leaveEff . runEitherDef . flip (evalStateEff @"vertices") vertices

lookupVertice :: (Lookup xs "vertices" (State (VerticeMap a)), MonadError ConvertError (Eff xs)) => String -> Eff xs (G.Vertice a)
lookupVertice x = getsEff #vertices (Map.lookup x) >>= \case
  Nothing -> throwError $ UndefinedNode x
  Just v -> pure v
syntaxToGraph :: Ord a => Term -> Eff (ConvertEff a) (GraphWithOutput a)
syntaxToGraph (Variable x) = do
  vertice <- lookupVertice x
  pure (vertice, \out -> G.newGraph [vertice, out] [G.newEdge vertice out])
syntaxToGraph (Application f x) = do
  (input, graphf) <- syntaxToGraph f
  (_, graphx) <- syntaxToGraph x
  pure (input, \out -> graphx input `G.union` graphf out) -- overlay
