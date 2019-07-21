module Functory.Syntax.MinimalSpec (spec) where

import Functory.Syntax.Minimal
import RIO
import Test.Hspec

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
  it "f (g x)" . example $ do
    let ast = Application (Variable "f") (Application (Variable "g") (Variable "x"))
    return ()
