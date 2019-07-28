module Main where

import Functory.Dot
import Functory.Graph
import Functory.Graph.Minimal
import Functory.Syntax.Minimal
import RIO
import qualified RIO.Map as Map
import Prelude (print)

main :: IO ()
main = test

test = do
  print "test"
  let x = Variable "x"
      y = Variable "y"
      f = Variable "f"
      g = Variable "g"
      ast = Application (Application g (Application f x)) y
      vertexx = Vertex "x"
      vertexy = Vertex "y"
      vertexf = Vertex "f"
      vertexg = Vertex "g"
      out = Vertex "out"
      vertices = Map.fromList [("x", vertexx), ("y", vertexy), ("f", vertexf), ("g", vertexg)]
  case runConvertEff vertices (callGraph ast) of
    Left e -> return ()
    Right (_, graph) -> writeGraph "test" $ graph out
 