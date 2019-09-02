module Functory.GraphSpec (spec) where

import Data.Extensible
import Functory.Graph
import RIO
import Test.Hspec

spec :: Spec
spec = visualizeSpec

visualizeSpec :: Spec
visualizeSpec = describe "visualize test" $ do
  it "*" $ do
    let out = Vertex "out"
        graph = newGraph [out] []
        expected = "*"
    visualize out graph `shouldBe` VisualizedGraph (#vertex @= out <: #children @= [] <: nil)

  it "* --> *" $ do
    let out = Vertex "out"
        one = Vertex "one"
        graph = newGraph [out, one] [newEdge one out]
    visualize out graph `shouldBe` VisualizedGraph (#vertex @= out <: #children @= [VisualizedGraph (#vertex @= one <: #children @= [] <: nil)] <: nil)

  it "* --> * --> *" $ do
    let out = Vertex "out"
        one = Vertex "one"
        two = Vertex "two"
        graph = newGraph [out, one, two] [newEdge one out, newEdge two one]
    visualize out graph `shouldBe` VisualizedGraph (#vertex @= out <: #children @= [VisualizedGraph (#vertex @= one <: #children @= [VisualizedGraph (#vertex @= two <: #children @= [] <: nil)] <: nil)] <: nil)

  it "* | * --> *" $ do
    let out = Vertex "out"
        one = Vertex "one"
        two = Vertex "two"
        graph = newGraph [out, one, two] [newEdge one out, newEdge two out]
    visualize out graph `shouldBe` VisualizedGraph (#vertex @= out <: #children @= [VisualizedGraph (#vertex @= one <: #children @= [] <: nil), VisualizedGraph (#vertex @= two <: #children @= [] <: nil)] <: nil)

  it "* --> * | * --> *" $ do
    let out = Vertex "out"
        one = Vertex "one"
        two = Vertex "two"
        three = Vertex "three"
        graph = newGraph [out, one, two, three] [newEdge one out, newEdge two out, newEdge three two]
    visualize out graph `shouldBe` VisualizedGraph (#vertex @= out <: #children @= [VisualizedGraph (#vertex @= one <: #children @= [] <: nil), VisualizedGraph (#vertex @= two <: #children @= [VisualizedGraph (#vertex @= three <: #children @= [] <: nil)] <: nil)] <: nil)
