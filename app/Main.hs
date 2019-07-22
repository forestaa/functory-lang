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
      verticex = Vertice "x"
      verticey = Vertice "y"
      verticef = Vertice "f"
      verticeg = Vertice "g"
      out = Vertice "out"
      vertices = Map.fromList [("x", verticex), ("y", verticey), ("f", verticef), ("g", verticeg)]
  case runConvertEff vertices (syntaxToGraph ast) of
    Left e -> return ()
    Right (_, graph) -> writeGraph "test" $ graph out
 