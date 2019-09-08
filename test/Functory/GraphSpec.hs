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
        graph = newGraph [out] [] :: Graph Int String
    visualize out graph `shouldBe` VisualizedGraph (#vertex @= out <: #children @= [] <: nil)

  it "* --> *" $ do
    let out = Vertex "out"
        one = Vertex "one"
        item = 0
        graph = newGraph [out, one] [newEdge item one out]
    visualize out graph `shouldBe` VisualizedGraph (#vertex @= out <: #children @= [(item, VisualizedGraph (#vertex @= one <: #children @= [] <: nil))] <: nil)

  it "* --> * --> *" $ do
    let out = Vertex "out"
        one = Vertex "one"
        two = Vertex "two"
        itemOneOut = 0
        itemTwoOne = 1
        graph = newGraph [out, one, two] [newEdge itemOneOut one out, newEdge itemTwoOne two one]
    visualize out graph `shouldBe` VisualizedGraph (#vertex @= out <: #children @= [(itemOneOut, VisualizedGraph (#vertex @= one <: #children @= [(itemTwoOne, VisualizedGraph (#vertex @= two <: #children @= [] <: nil))] <: nil))] <: nil)

  it "* | * --> *" $ do
    let out = Vertex "out"
        one = Vertex "one"
        two = Vertex "two"
        itemOneOut = 0
        itemTwoOut = 1
        graph = newGraph [out, one, two] [newEdge itemOneOut one out, newEdge itemTwoOut two out]
    visualize out graph `shouldBe` VisualizedGraph (#vertex @= out <: #children @= [(itemOneOut, VisualizedGraph (#vertex @= one <: #children @= [] <: nil)), (itemTwoOut, VisualizedGraph (#vertex @= two <: #children @= [] <: nil))] <: nil)

  it "* --> * | * --> *" $ do
    let out = Vertex "out"
        one = Vertex "one"
        two = Vertex "two"
        three = Vertex "three"
        itemOneOut = 0
        itemTwoOut = 1
        itemThreeTwo = 2
        graph = newGraph [out, one, two, three] [newEdge itemOneOut one out, newEdge itemTwoOut two out, newEdge itemThreeTwo three two]
    visualize out graph `shouldBe` VisualizedGraph (#vertex @= out <: #children @= [(itemOneOut, VisualizedGraph (#vertex @= one <: #children @= [] <: nil)), (itemTwoOut, VisualizedGraph (#vertex @= two <: #children @= [(itemThreeTwo, VisualizedGraph (#vertex @= three <: #children @= [] <: nil))] <: nil))] <: nil)
