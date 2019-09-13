module CLI.Graph where

import Control.Monad.Except
import Control.Monad.Error.Class
import Data.Aeson
import Data.Extensible
import qualified Data.Bifunctor as Bi
import Data.ByteString.Lazy (ByteString)
import Data.Maybe
import Functory.Dot
import qualified Functory.Graph as G
import Functory.Graph.SimplyTypedLambda
import Functory.Parser.SimplyTypedLambda
import qualified Functory.Syntax.Minimal as Minimal
import Functory.Syntax.SimplyTypedLambda hiding (typingNamedTerm)
import RIO hiding (ByteString, DecodeError)
import qualified RIO.List.Partial as List
import qualified RIO.Map as Map
import qualified RIO.Map.Partial as Map
import qualified RIO.Vector as V
import System.Process
import qualified Text.Parsec as P


newtype CompileResult = CompileResult (Record '["status" >: Int, "output" >: Maybe CompileOutput, "description" >: Maybe String]) deriving (ToJSON, FromJSON)
compile :: (Monad m, Dot m String String) => ByteString -> m ByteString
compile s = do
  result <- runExceptT $ compileInternal s
  case result of
    Right output -> pure . encode . CompileResult $ #status @= 0 <: #output @= Just output <: #description @= Nothing <: nil
    Left e -> do
      let (status, desc) = errorDesc e
      pure . encode . CompileResult $ #status @= status <: #output @= Nothing <: #description @= Just desc <: nil
  where
    errorDesc DecodeError = (1, "failed to decode")
    errorDesc (ParseError e) = (2, show e)
    errorDesc (CallGraphError e) = (3, show e)

class (Ord a, Ord b, Show a, Show b) => Dot m a b where
  writeDotGraph :: String -> String -> G.Graph a b -> m ()

instance (Ord a, Ord b, Show a, Show b) => Dot IO a b where
  writeDotGraph functoryID graphName graph = do
    writeGraph functoryID graphName graph
    callProcess "dot" ["-Tpng", "-o", "images/" ++ functoryID ++ ".png", "images/" ++ functoryID ++ ".dot"] `catch` (\(e :: IOException) -> traceShowIO e)
instance (Ord a, Ord b, Show a, Show b) => Dot Identity a b where
  writeDotGraph _ _ _ = pure ()

newtype CompileInput = CompileInput (Record '["functory_id" >: String, "source" >: String]) deriving (Show, Eq, FromJSON, ToJSON)
data CompileErrors =
    DecodeError
  | ParseError P.ParseError
  | CallGraphError CallGraphError
compileInternal :: (Monad m, Dot m String String) => ByteString -> ExceptT CompileErrors m CompileOutput
compileInternal s = do
  CompileInput input <- maybe (throwError DecodeError) pure $ decode s
  (name, rawTerm, populatedTerm, ctx) <- liftEither . Bi.first ParseError $ P.runParser funcDefinition () "" (input ^. #source)
  ty <- liftEither . Bi.first CallGraphError $ typingNamedTerm context rawTerm
  (_, graphWithOutput) <- liftEither . Bi.first CallGraphError $ runCallGraph (Map.union vertices (Map.fromList (fmap ((id &&& G.Vertex . show) . fst) ctx))) items (contextMinimal V.++ V.fromList (fmap (second toMinimalType) ctx)) (context V.++ V.fromList (fmap (second VariableBind) ctx)) (callGraph populatedTerm)
  let out = G.Vertex "out"
      graph = graphWithOutput out
      functoryID = input ^. #functory_id
  lift $ writeDotGraph functoryID name graph
  let visualizedGraph = convertFromVisualizedGraph materialMap functionMap . G.visualize out $ graphWithOutput out
  pure . CompileOutput $ #name @= name <: #type @= ty <: #timer @= 10 <: #graph @= visualizedGraph <: nil
  where
    vertices = Map.fromList [
        ("slash", G.Vertex "slash")
      , ("fire", G.Vertex "fire")
      ]
    items = Map.fromList [
        (Minimal.Unit, "()")
      , (Minimal.Constant "Wood", "Wood")
      , (Minimal.Constant "Water", "Water")
      , (Minimal.Constant "Energy", "Energy")
      , (Minimal.Constant "HotWater", "HotWater")
      ]
    context = V.fromList [
        ("slash", VariableBind $ Arrow (Constant "Wood") (Constant "Energy"))
      , ("fire", VariableBind $ Arrow (Constant "Energy") (Arrow (Constant "Water") (Constant "HotWater")))
      ]
    contextMinimal = fmap (second (\(VariableBind ty) -> toMinimalType ty)) context
    materialMap = Map.fromList [
        ("Wood", 1)
      , ("Water", 2)
      , ("Energy", 3)
      , ("HotWater", 4)
      ]
    functionMap = Map.fromList [
        ("slash", 1)
      , ("fire", 2)
      , ("wood", 3)
      , ("water", 4)
      ]

newtype CompileOutput = CompileOutput (Record '["name" >: String, "type" >: Type, "timer" >: Int, "graph" >: Graph]) deriving (Show, Eq, ToJSON, FromJSON)
newtype Graph = Graph (Record '["output" >: Vertex]) deriving (Show, Eq, FromJSON, ToJSON)
newtype Vertex = Vertex  (Record '["item" >: Int, "children" >: [Edge]] ) deriving (Show, Eq, FromJSON, ToJSON)
newtype Edge = Edge (Record '["item" >: Int, "target" >: Vertex ]) deriving (Show, Eq, FromJSON, ToJSON)

convertFromVisualizedGraph :: (Map.Map String Int) -> (Map.Map String Int) -> G.VisualizedGraph String String -> Graph
convertFromVisualizedGraph materialMap functionMap g = Graph (#output @= convertFromVisualizedGraphOutput materialMap functionMap g <: nil)
convertFromVisualizedGraphOutput :: (Map.Map String Int) -> (Map.Map String Int) -> G.VisualizedGraph String String -> Vertex
convertFromVisualizedGraphOutput materialMap functionMap (G.VisualizedGraph g) =
  Vertex $ #item @= materialMap Map.! (fst (List.head (g ^. #children))) <: #children @= fmap (\(item, g) -> Edge (#item @= materialMap Map.! item <: #target @= convertFromVisualizedGraphInternal materialMap functionMap g <: nil)) (g ^. #children) <: nil
convertFromVisualizedGraphInternal :: (Map.Map String Int) -> (Map.Map String Int) -> G.VisualizedGraph String String -> Vertex
convertFromVisualizedGraphInternal materialMap functionMap (G.VisualizedGraph g) =
    Vertex $ #item @= functionMap Map.! (extract (g ^. #vertex)) <: #children @= fmap ((\(item, v) -> Edge (#item @= materialMap Map.! item <: #target @= v <: nil)) . second (convertFromVisualizedGraphInternal materialMap functionMap)) (g ^. #children) <: nil
  where
    extract (G.Vertex a) = a

