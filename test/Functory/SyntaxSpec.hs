module Functory.SyntaxSpec where

import Test.Hspec
import Functory.Syntax


spec :: Spec
spec = syntaxSpec

syntaxSpec :: Spec
syntaxSpec = describe "ast samples" $ do
  it "x" . example $ do
    let ast = Variable "x"
    return ()
  it "f x" . example $ do
    let ast = Application (Variable "f") (Variable "x") 
    return ()
  it "let a = f x in a" . example $ do
    let ast = Let "a" (Application (Variable "x") (Variable "x")) (Variable "a")
    return ()
