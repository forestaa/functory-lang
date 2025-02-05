module Functory.Graph.MinimalSpec where

import Functory.Graph
import Functory.Graph.Minimal
import Functory.Syntax.Minimal
import RIO
import qualified RIO.Map as Map
import qualified RIO.Vector as V
import Test.Hspec


spec :: Spec
spec = callGraphSpec

callGraphSpec :: Spec
callGraphSpec = describe "callGraph test" $ do
  it "x" $ do
    let ast      = Variable "x"
        vertexx  = Vertex "x"
        out      = Vertex "out"
        vertices = Map.fromList [("x", vertexx)]
        items = Map.fromList [(Unit, 0)]
        ctx = V.fromList [("x", Unit)]
    case runConvertEff vertices items ctx (callGraph ast) of
      Left e -> expectationFailure (show e)
      Right (_, graph) -> graph out `shouldBe` newGraph [vertexx, out] [newEdge 0 vertexx out]

  it "f x" $ do
    let x = Variable "x"
        f = Variable "f"
        ast = Application f x
        vertexx = Vertex "x"
        vertexf = Vertex "f"
        out = Vertex "out"
        vertices = Map.fromList [("x", vertexx), ("f", vertexf)]
        items = Map.fromList [(Unit, 0)]
        ctx = V.fromList [("x", Unit), ("f", Arrow Unit Unit)]
    case runConvertEff vertices items ctx (callGraph ast) of
      Left e -> expectationFailure (show e)
      Right (_, graph) -> graph out `shouldBe` newGraph [vertexx, vertexf, out] [newEdge 0 vertexx vertexf, newEdge 0 vertexf out]

  it "g (f x)" $ do
    let x = Variable "x"
        f = Variable "f"
        g = Variable "g"
        ast = Application g (Application f x)
        vertexx = Vertex "x"
        vertexf = Vertex "f"
        vertexg = Vertex "g"
        out = Vertex "out"
        vertices = Map.fromList [("x", vertexx), ("f", vertexf), ("g", vertexg)]
        items = Map.fromList [(Unit, 0)]
        ctx = V.fromList [("x", Unit), ("f", Arrow Unit Unit), ("g", Arrow Unit Unit)]
    case runConvertEff vertices items ctx (callGraph ast) of
      Left e -> expectationFailure (show e)
      Right (_, graph) -> graph out `shouldBe` newGraph [vertexx, vertexf, vertexg, out] [newEdge 0 vertexx vertexf, newEdge 0 vertexf vertexg, newEdge 0 vertexg out]

  it "g (f x y)" $ do
    let x = Variable "x"
        y = Variable "y"
        f = Variable "f"
        g = Variable "g"
        ast = Application g (Application (Application f x) y)
        vertexx = Vertex "x"
        vertexy = Vertex "y"
        vertexf = Vertex "f"
        vertexg = Vertex "g"
        out = Vertex "out"
        vertices = Map.fromList [("x", vertexx), ("y", vertexy), ("f", vertexf), ("g", vertexg)]
        items = Map.fromList [(Unit, 0)]
        ctx = V.fromList [("x", Unit), ("y", Unit), ("f", Arrow Unit (Arrow Unit Unit)), ("g", Arrow Unit Unit)]
    case runConvertEff vertices items ctx (callGraph ast) of
      Left e -> expectationFailure (show e)
      Right (_, graph) -> graph out `shouldBe` newGraph [vertexx, vertexy, vertexf, vertexg, out] [newEdge 0 vertexx vertexf, newEdge 0 vertexy vertexf, newEdge 0 vertexf vertexg, newEdge 0 vertexg out]

  it "g (f x) y" $ do
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
        items = Map.fromList [(Unit, 0)]
        ctx = V.fromList [("x", Unit), ("y", Unit), ("f", Arrow Unit Unit), ("g", Arrow Unit (Arrow Unit Unit))]
    case runConvertEff vertices items ctx (callGraph ast) of
      Left e -> expectationFailure (show e)
      Right (_, graph) -> graph out `shouldBe` newGraph [vertexx, vertexy, vertexf, vertexg, out] [newEdge 0 vertexx vertexf, newEdge 0 vertexy vertexg, newEdge 0 vertexf vertexg, newEdge 0 vertexg out]
