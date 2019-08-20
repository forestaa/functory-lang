module Functory.Graph.SimplyTypedLambdaSpec (spec) where

import Data.Extensible
import Functory.Graph
import Functory.Graph.SimplyTypedLambda
import Functory.Syntax.SimplyTypedLambda
import RIO
import qualified RIO.Map as Map
import qualified RIO.Vector as V
import Test.Hspec

spec :: Spec
spec = callGraphSpec

callGraphSpec :: Spec
callGraphSpec = describe "callGraph test" $ do
  it "x" $ do
    let term = Constant "x"
        vertexx = Vertex "x"
        out = Vertex "out"
        ctx = V.fromList [("x", ConstantBind Unit)]
        vertices = Map.fromList [("x", vertexx)]
    case runCallGraph vertices ctx (callGraph term) of
      Left e -> expectationFailure (show e)
      Right (_, graph) -> graph out `shouldBe` newGraph [vertexx, out] [newEdge vertexx out]

  it "f x" $ do
    let x = Constant "x"
        f = Constant "f" 
        term = Application (#function @= f <: #argument @= x <: nil)
        vertexx = Vertex "x"
        vertexf = Vertex "f"
        out = Vertex "out"
        ctx = V.fromList [("x", ConstantBind Unit), ("f", ConstantBind (Arrow Unit Unit))]
        vertices = Map.fromList [("x", vertexx), ("f", vertexf)]
    case runCallGraph vertices ctx (callGraph term) of
      Left e -> expectationFailure (show e)
      Right (_, graph) -> graph out `shouldBe` newGraph [vertexx, vertexf, out] [newEdge vertexx vertexf, newEdge vertexf out]

  it "g (f x)" $ do
    let x = Constant "x"
        f = Constant "f"
        g = Constant "g"
        term = Application (#function @= g <: #argument @= Application (#function @= f <: #argument @= x <: nil) <: nil)
        vertexx = Vertex "x"
        vertexf = Vertex "f"
        vertexg = Vertex "g"
        out = Vertex "out"
        ctx = V.fromList [("x", ConstantBind Unit), ("f", ConstantBind (Arrow Unit Unit)), ("g", ConstantBind (Arrow Unit Unit))]
        vertices = Map.fromList [("x", vertexx), ("f", vertexf), ("g", vertexg)]
    case runCallGraph vertices ctx (callGraph term) of
      Left e -> expectationFailure (show e)
      Right (_, graph) -> graph out `shouldBe` newGraph [vertexx, vertexf, vertexg, out] [newEdge vertexx vertexf, newEdge vertexf vertexg, newEdge vertexg out]

  it "g (f x y)" $ do
    let x = Constant "x"
        y = Constant "y"
        f = Constant "f"
        g = Constant "g"
        term = Application (#function @= g <: #argument @= Application (#function @= Application (#function @= f <: #argument @= x <: nil) <: #argument @= y <: nil) <: nil)
        vertexx = Vertex "x"
        vertexy = Vertex "y"
        vertexf = Vertex "f"
        vertexg = Vertex "g"
        out = Vertex "out"
        ctx = V.fromList [("x", ConstantBind Unit), ("y", ConstantBind Unit), ("f", ConstantBind (Arrow Unit (Arrow Unit Unit))), ("g", ConstantBind (Arrow Unit Unit))]
        vertices = Map.fromList [("x", vertexx), ("y", vertexy), ("f", vertexf), ("g", vertexg)]
    case runCallGraph vertices ctx (callGraph term) of
      Left e -> expectationFailure (show e)
      Right (_, graph) -> graph out `shouldBe` newGraph [vertexx, vertexy, vertexf, vertexg, out] [newEdge vertexx vertexf, newEdge vertexy vertexf, newEdge vertexf vertexg, newEdge vertexg out]

  it "g (f x) y" $ do
    let x = Constant "x"
        y = Constant "y"
        f = Constant "f"
        g = Constant "g"
        term = Application (#function @= (Application (#function @= g <: #argument @= Application (#function @= f <: #argument @= x <: nil) <: nil)) <: #argument @= y <: nil)
        vertexx = Vertex "x"
        vertexy = Vertex "y"
        vertexf = Vertex "f"
        vertexg = Vertex "g"
        out = Vertex "out"
        ctx = V.fromList [("x", ConstantBind Unit), ("y", ConstantBind Unit), ("f", ConstantBind (Arrow Unit Unit)), ("g", ConstantBind (Arrow Unit (Arrow Unit Unit)))]
        vertices = Map.fromList [("x", vertexx), ("y", vertexy), ("f", vertexf), ("g", vertexg)]
    case runCallGraph vertices ctx (callGraph term) of
      Left e -> expectationFailure (show e)
      Right (_, graph) -> graph out `shouldBe` newGraph [vertexx, vertexy, vertexf, vertexg, out] [newEdge vertexx vertexf, newEdge vertexy vertexg, newEdge vertexf vertexg, newEdge vertexg out]

  it "(λx.λy. g x y) (f x) y -> g (f x) y" $ do
    let x = Constant "x"
        y = Constant "y"
        f = Constant "f"
        g = Constant "g"
        term = Application (#function @= Application (#function @= (Abstraction $ #name @= "x" <: #type @= Unit <: #body @= Abstraction (#name @= "y" <: #type @= Unit <: #body @= Application (#function @= Application (#function @= g <: #argument @= Variable "x" <: nil) <: #argument @= Variable "y" <: nil) <: nil) <: nil) <: #argument @= Application (#function @= f <: #argument @= x <: nil) <: nil) <: #argument @= y <: nil)
        vertexx = Vertex "x"
        vertexy = Vertex "y"
        vertexf = Vertex "f"
        vertexg = Vertex "g"
        out = Vertex "out"
        ctx = V.fromList [("x", ConstantBind Unit), ("y", ConstantBind Unit), ("f", ConstantBind (Arrow Unit Unit)), ("g", ConstantBind (Arrow Unit (Arrow Unit Unit)))]
        vertices = Map.fromList [("x", vertexx), ("y", vertexy), ("f", vertexf), ("g", vertexg)]
    case runCallGraph vertices ctx (callGraph term) of
      Left e -> expectationFailure (show e)
      Right (_, graph) -> graph out `shouldBe` newGraph [vertexx, vertexy, vertexf, vertexg, out] [newEdge vertexx vertexf, newEdge vertexy vertexg, newEdge vertexf vertexg, newEdge vertexg out]

  it "λx.x" $ do
    let term = Abstraction $ #name @= "x" <: #type @= Unit <: #body @= Variable "x" <: nil
        out = Vertex "out"
        ctx = V.fromList []
        vertices = Map.fromList []
    case runCallGraph vertices ctx (callGraph term) of 
      Left e -> e `shouldBe` (CallGraphSimpleError (TypeIsNotUnit (Arrow Unit Unit)))
      Right (_, graph) -> expectationFailure $ concat ["should raise CallGraphSimpleError: return graph = ", show (graph out)]