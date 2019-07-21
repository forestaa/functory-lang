module Functory.Graph.MinimalSpec where

import Functory.Graph
import Functory.Graph.Minimal
import Functory.Syntax.Minimal
import RIO
import qualified RIO.Map as Map
import Test.Hspec

spec :: Spec
spec = syntaxToGraphSpec

syntaxToGraphSpec :: Spec
syntaxToGraphSpec = describe "syntaxToGraph test" $ do
  it "x" $ do
    let ast      = Variable "x"
        verticex = Vertice "x"
        out      = Vertice "out"
        vertices = Map.fromList [("x", verticex)]
    case runConvertEff vertices (syntaxToGraph ast) of
      Left e -> expectationFailure (show e)
      Right (_, graph) -> graph out `shouldBe` newGraph [verticex, out] [newEdge verticex out]

  it "f x" $ do
    let x = Variable "x"
        f = Variable "f"
        ast = Application f x
        verticex = Vertice "x"
        verticef = Vertice "f"
        out = Vertice "out"
        vertices = Map.fromList [("x", verticex), ("f", verticef)]
    case runConvertEff vertices (syntaxToGraph ast) of
      Left e -> expectationFailure (show e)
      Right (_, graph) -> graph out `shouldBe` newGraph [verticex, verticef, out] [newEdge verticex verticef, newEdge verticef out]

  it "g (f x)" $ do
    let x = Variable "x"
        f = Variable "f"
        g = Variable "g"
        ast = Application g (Application f x)
        verticex = Vertice "x"
        verticef = Vertice "f"
        verticeg = Vertice "g"
        out = Vertice "out"
        vertices = Map.fromList [("x", verticex), ("f", verticef), ("g", verticeg)]
    case runConvertEff vertices (syntaxToGraph ast) of
      Left e -> expectationFailure (show e)
      Right (_, graph) -> graph out `shouldBe` newGraph [verticex, verticef, verticeg, out] [newEdge verticex verticef, newEdge verticef verticeg, newEdge verticeg out]

  it "g (f x y)" $ do
    let x = Variable "x"
        y = Variable "y"
        f = Variable "f"
        g = Variable "g"
        ast = Application g (Application (Application f x) y)
        verticex = Vertice "x"
        verticey = Vertice "y"
        verticef = Vertice "f"
        verticeg = Vertice "g"
        out = Vertice "out"
        vertices = Map.fromList [("x", verticex), ("y", verticey), ("f", verticef), ("g", verticeg)]
    case runConvertEff vertices (syntaxToGraph ast) of
      Left e -> expectationFailure (show e)
      Right (_, graph) -> graph out `shouldBe` newGraph [verticex, verticey, verticef, verticeg, out] [newEdge verticex verticef, newEdge verticey verticef, newEdge verticef verticeg, newEdge verticeg out]

  it "g (f x) y" $ do
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
      Left e -> expectationFailure (show e)
      Right (_, graph) -> graph out `shouldBe` newGraph [verticex, verticey, verticef, verticeg, out] [newEdge verticex verticef, newEdge verticey verticeg, newEdge verticef verticeg, newEdge verticeg out]
