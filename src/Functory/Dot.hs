module Functory.Dot (writeGraph) where

import Prelude (writeFile)
import Functory.Graph

import RIO
import qualified RIO.Map as Map
import qualified RIO.Map.Partial as Map
import qualified RIO.Set as Set


graphToDot :: (Ord b, Show a, Show b) => String -> Graph a b -> Reader (Map.Map (Vertex b) Int) String
graphToDot s (Graph graph) = do
  nodes <- verticesToDot $ graph ^. #vertices
  es <- edgesToDot $ graph ^. #edges
  return . unlines . fmap ("  " ++) $ concat [["label = " `mappend` show s `mappend` ";","", "//node define"], nodes, ["","//edge define"], es]

verticesToDot :: (Ord b, Show b) => Set.Set (Vertex b) -> Reader (Map.Map (Vertex b) Int) [String]
verticesToDot vs = (\intMap -> Set.toList $ Set.map (\v -> concat [show (intMap Map.! v), " [label = ", show $ show v, "];"]) vs) <$> ask

edgesToDot :: (Ord b, Show a, Show b) => Set.Set (Edge a b) -> Reader (Map.Map (Vertex b) Int) [String]
edgesToDot es = (\intMap -> Set.toList $ Set.map (\(Edge e) -> concat [show $ intMap Map.! (e ^. #source), " -> ", show $ intMap Map.! (e ^. #target), " [label = ", show $ show e,"];"]) es) <$> ask


writeGraph :: (Ord b, Show a, Show b) => String -> Graph a b -> IO ()
writeGraph file g@(Graph graph) = writeGraphInternal file $ runReader (graphToDot file g) . Map.fromList $ zip (Set.toList $ graph ^. #vertices) [1..]

writeGraphInternal :: String -> String -> IO ()
writeGraphInternal file content = writeFile (concat ["images/", file, ".dot"]) $ concat ["digraph {", content, "}"]
