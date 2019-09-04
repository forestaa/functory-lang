module Functory.Graph.Minimal where

import qualified Data.Bifunctor as Bi
import Data.Extensible
import Functory.Graph
import Functory.Syntax.Minimal
import RIO
import qualified RIO.Map as Map
import SString


type GraphWithOutput a b =  (Vertex b, Vertex b -> Graph a b)
type VertexMap b = Map.Map SString (Vertex b)
type TypeItemMap a = Map.Map Type a
data ConvertError =
    UndefinedNode SString
  | UndefinedEdge Type
  | TypingError TypingError
  deriving (Show, Eq)
runConvertEff :: VertexMap b
  -> TypeItemMap a
  -> VariableContext
  -> Eff '[
      "vertices" >: ReaderEff (VertexMap b)
    , "typeItem" >: ReaderEff (TypeItemMap a)
    , "callGraphMinimal" >: EitherEff ConvertError
    , "contextMinimal" >: ReaderEff VariableContext
    , "typingMinimal" >: EitherEff TypingError
    ] c
  -> Either ConvertError c
runConvertEff vertices items ctx = join . leaveEff . fmap (Bi.first TypingError) . runEitherEff @"typingMinimal" . flip (runReaderEff @"contextMinimal") ctx . runEitherEff @"callGraphMinimal" . flip (runReaderEff @"typeItem") items . flip (runReaderEff @"vertices") vertices

lookupVertice :: (
    Lookup xs "vertices" (ReaderEff (VertexMap b))
  , Lookup xs "callGraphMinimal" (EitherEff ConvertError)
  ) => SString -> Eff xs (Vertex b)
lookupVertice x = asksEff #vertices (Map.lookup x) >>= \case
  Just v -> pure v
  Nothing -> throwEff #callGraphMinimal $ UndefinedNode x


lookupEdgeItem :: (
    Lookup xs "typeItem" (ReaderEff (TypeItemMap a))
  , Lookup xs "callGraphMinimal" (EitherEff ConvertError)
  , Lookup xs "contextMinimal" (ReaderEff VariableContext)
  , Lookup xs "typingMinimal" (EitherEff TypingError)
  ) => SString -> Eff xs a
lookupEdgeItem x = do
  ty <- codomain <$> typing (Variable x)
  asksEff #typeItem (Map.lookup ty) >>= \case
    Just a -> pure a
    Nothing -> throwEff #callGraphMinimal $ UndefinedEdge ty
  where
    codomain Unit = Unit
    codomain (Arrow _ ty) = codomain ty

callGraph :: (
    Lookup xs "vertices" (ReaderEff (VertexMap b))
  , Lookup xs "typeItem" (ReaderEff (TypeItemMap a))
  , Lookup xs "callGraphMinimal" (EitherEff ConvertError)
  , Lookup xs "contextMinimal" (ReaderEff VariableContext)
  , Lookup xs "typingMinimal" (EitherEff TypingError)
  , Ord a
  , Ord b)
  => Term
  -> Eff xs (GraphWithOutput a b)
callGraph (Variable x) = do
  item <- lookupEdgeItem x
  vertice <- lookupVertice x
  pure (vertice, \out -> newGraph [vertice, out] [newEdge item vertice out])
callGraph (Application f x) = do
  (input, graphf) <- callGraph f
  (_, graphx) <- callGraph x
  pure (input, \out -> graphx input `union` graphf out) -- overlay
