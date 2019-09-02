module CLI.Graph where

import Control.Monad.Error.Class
import Data.Aeson
import Data.Extensible
import Data.ByteString.Lazy (ByteString)
import qualified Functory.Graph as G
import Functory.Graph.SimplyTypedLambda
import Functory.Parser.SimplyTypedLambda
import Functory.Syntax.SimplyTypedLambda
import RIO hiding (ByteString)
import qualified RIO.Map as Map
import qualified RIO.Vector as V
import qualified Text.Parsec as P

newtype CompileInput = CompileInput (Record '["source" >: String]) deriving (Show, Eq, FromJSON, ToJSON)
data HogeErrors = FailedToDecode | ParseError | CallGraphError
hoge :: ByteString -> Either HogeErrors ByteString
hoge s = do
  case decode s of
    Nothing -> throwError FailedToDecode
    Just (CompileInput input) -> case P.runParser funcDefinition () "" (input ^. #source) of
      Left e -> throwError ParseError
      Right (name, term) -> case runCallGraph Map.empty V.empty (callGraph term) of
        Left e -> throwError CallGraphError
        Right (_, graph) -> do
          let out = G.Vertex 0
          pure . encode . convertFromVisualizedGraph . G.visualize out $ graph out



newtype Graph = Graph (Record '["output" >: Vertex]) deriving (Show, Eq, FromJSON, ToJSON)
newtype Vertex = Vertex  (Record '["item" >: Int, "children" >: [Edge]] ) deriving (Show, Eq, FromJSON, ToJSON)
newtype Edge = Edge (Record '["item" >: Int, "target" >: Vertex ]) deriving (Show, Eq, FromJSON, ToJSON)

convertFromVisualizedGraph :: G.VisualizedGraph Int -> Graph
convertFromVisualizedGraph g = Graph (#output @= convertFromVisualizedGraphInternal g <: nil)
convertFromVisualizedGraphInternal :: G.VisualizedGraph Int -> Vertex
convertFromVisualizedGraphInternal (G.VisualizedGraph g) = Vertex $ #item @= extract (g ^. #vertex) <: #children @= fmap ((\v -> Edge (#item @= 0 <: #target @= v <: nil)) . convertFromVisualizedGraphInternal) (g ^. #children) <: nil
  where
    extract (G.Vertex a) = a
