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
import Functory.Syntax.SimplyTypedLambda
import RIO hiding (ByteString, DecodeError)
import qualified RIO.Map as Map
import qualified RIO.Vector as V
import System.Process
import qualified Text.Parsec as P


newtype CompileResult = CompileResult (Record '["status" >: Int, "output" >: Maybe CompileOutput, "description" >: Maybe String]) deriving (ToJSON, FromJSON)
compile :: ByteString -> IO ByteString
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

newtype CompileInput = CompileInput (Record '["functory_id" >: String, "source" >: String]) deriving (Show, Eq, FromJSON, ToJSON)
data CompileErrors =
    DecodeError
  | ParseError P.ParseError
  | CallGraphError CallGraphError
compileInternal :: ByteString -> ExceptT CompileErrors IO CompileOutput
compileInternal s = do
  CompileInput input <- maybe (throwError DecodeError) pure $ decode s
  (name, term) <- liftEither . Bi.first ParseError $ P.runParser funcDefinition () "" (input ^. #source)
  ((_, graphWithOutput), ty) <- liftEither . Bi.first CallGraphError $ runCallGraph vertices items contextMinimal context (callGraph term)
  let out = G.Vertex 0
      graph = graphWithOutput out
      functoryID = input ^. #functory_id
  lift $ writeGraph functoryID graph
  lift $ callProcess "dot" ["-Tpng", "-o", "images/" ++ functoryID ++ ".png", "images/" ++ functoryID ++ ".dot"] `catch` (\(e :: IOException) -> traceShowIO e)
  let visualizedGraph = convertFromVisualizedGraph . G.visualize out $ graphWithOutput out
  pure . CompileOutput $ #name @= name <: #type @= ty <: #timer @= 10 <: #graph @= visualizedGraph <: nil
  where
    vertices = Map.fromList [("slash", G.Vertex 1), ("fire", G.Vertex 2)]
    items = Map.fromList [(Minimal.Unit, 3)]
    contextMinimal = V.fromList [("slash", Minimal.Arrow Minimal.Unit Minimal.Unit), ("fire", Minimal.Arrow Minimal.Unit (Minimal.Arrow Minimal.Unit Minimal.Unit))]
    context = V.fromList [("slash", VariableBind $ Arrow Unit Unit), ("fire", VariableBind $ Arrow Unit (Arrow Unit Unit))]

newtype CompileOutput = CompileOutput (Record '["name" >: String, "type" >: Type, "timer" >: Int, "graph" >: Graph]) deriving (Show, Eq, ToJSON, FromJSON)
newtype Graph = Graph (Record '["output" >: Vertex]) deriving (Show, Eq, FromJSON, ToJSON)
newtype Vertex = Vertex  (Record '["item" >: Int, "children" >: [Edge]] ) deriving (Show, Eq, FromJSON, ToJSON)
newtype Edge = Edge (Record '["item" >: Int, "target" >: Vertex ]) deriving (Show, Eq, FromJSON, ToJSON)

convertFromVisualizedGraph :: G.VisualizedGraph Int Int -> Graph
convertFromVisualizedGraph g = Graph (#output @= convertFromVisualizedGraphInternal g <: nil)
convertFromVisualizedGraphInternal :: G.VisualizedGraph Int Int -> Vertex
convertFromVisualizedGraphInternal (G.VisualizedGraph g) = Vertex $ #item @= extract (g ^. #vertex) <: #children @= fmap ((\(item, v) -> Edge (#item @= item <: #target @= v <: nil)) . second convertFromVisualizedGraphInternal) (g ^. #children) <: nil
  where
    extract (G.Vertex a) = a

