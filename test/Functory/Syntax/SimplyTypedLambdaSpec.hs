module Functory.Syntax.SimplyTypedLambdaSpec (spec) where


import Data.Extensible
import Functory.Syntax.SimplyTypedLambda
import qualified Functory.Syntax.Minimal as Minimal
import RIO
import qualified RIO.Vector as V
import Test.Hspec


spec :: Spec
spec = do
  evalSpec
  typingSpec
  toMinimalSpec

evalSpec :: Spec
evalSpec = describe "eval test" $ do
  it "x -> x" $ do
    let term = Variable "x"
        expected = term
        ctx = V.fromList [("x", VariableBind Unit)]
    case evalNamedTerm ctx term of
      Left e -> expectationFailure (show e)
      Right term' -> term' `shouldBe` expected

  it "λx.x -> λx. x" $ do
    let term = Abstraction $ #name @= "x" <: #type @= Unit <: #body @= Variable "x" <: nil
        expected = term
        ctx = V.fromList []
    case evalNamedTerm ctx term of
      Left e -> expectationFailure (show e)
      Right term' -> term' `shouldBe` expected

  it "f x -> f x" $ do
    let term = Application $ #function @= Variable "f" <: #argument @= Variable "x" <: nil
        expected = term
        ctx = V.fromList [("f", VariableBind (Arrow Unit Unit)), ("x", VariableBind Unit)]
    case evalNamedTerm ctx term of
      Left e -> expectationFailure (show e)
      Right term' -> term' `shouldBe` expected

  it "(λx.x) x -> x" $ do
    let term = Application $ #function @= (Abstraction $ #name @= "x" <: #type @= Unit <: #body @= Variable "x" <: nil) <: #argument @= Variable "x" <: nil
        expected = Variable "x"
        ctx = V.fromList [("x", VariableBind Unit)]
    case evalNamedTerm ctx term of
      Left e -> expectationFailure (show e)
      Right term' -> term' `shouldBe` expected

  it "(λx.λy. g x y) (f x) y -> g (f x) y" $ do
    let term = Application (#function @= Application (#function @= (Abstraction $ #name @= "x" <: #type @= Unit <: #body @= Abstraction (#name @= "y" <: #type @= Unit <: #body @= Application (#function @= Application (#function @= Variable "g" <: #argument @= Variable "x" <: nil) <: #argument @= Variable "y" <: nil) <: nil) <: nil) <: #argument @= Application (#function @= Variable "f" <: #argument @= Variable "x" <: nil) <: nil) <: #argument @= Variable "y" <: nil)
        expected = Application (#function @= Application (#function @= Variable "g" <: #argument @= Application (#function @= Variable "f" <: #argument @= Variable "x" <: nil) <: nil) <: #argument @= Variable "y" <: nil)
        ctx = V.fromList [("x", VariableBind Unit), ("y", VariableBind Unit), ("f", VariableBind (Arrow Unit Unit)), ("g", VariableBind (Arrow Unit (Arrow Unit Unit)))]
    case evalNamedTerm ctx term of
      Left e -> expectationFailure (show e)
      Right term' -> term' `shouldBe` expected


typingSpec :: Spec
typingSpec = describe "typing test" $ do
  it "x" $ do
    let term = Variable "x"
        expected = Unit
        ctx = V.fromList [("x", VariableBind Unit)]
    case typingNamedTerm ctx term of
      Left e -> expectationFailure (show e)
      Right ty -> ty `shouldBe` expected

  it "f x" $ do
    let term = Application (#function @= Variable "f" <: #argument @= Variable "x" <: nil)
        expected = Unit
        ctx = V.fromList [("f", VariableBind (Arrow Unit Unit)), ("x", VariableBind Unit)]
    case typingNamedTerm ctx term of
      Left e -> expectationFailure (show e)
      Right ty -> ty `shouldBe` expected

  it "(λx. x) y" $ do
    let term = Application (#function @= Abstraction (#name @= "x" <: #type @= Unit <: #body @= Variable "x" <: nil) <: #argument @= Variable "y" <: nil)
        expected = Unit
        ctx = V.fromList [("y", VariableBind Unit)]
    case typingNamedTerm ctx term of
      Left e -> expectationFailure (show e)
      Right ty -> ty `shouldBe` expected

  it "(λx.λy. g x y) (f x)" $ do
    let term = Application (#function @= Abstraction (#name @= "x" <: #type @= Unit <: #body @= Abstraction (#name @= "y" <: #type @= Unit <: #body @= Application (#function @= Application (#function @= Variable "g" <: #argument @= Variable "x" <: nil) <: #argument @= Variable "y" <: nil) <: nil) <: nil) <: #argument @= Application (#function @= Variable "f" <: #argument @= Variable "x" <: nil) <: nil)
        expected = Arrow Unit Unit
        ctx = V.fromList [("g", VariableBind (Arrow Unit (Arrow Unit Unit))), ("f", VariableBind (Arrow Unit Unit)), ("x", VariableBind Unit)]
    case typingNamedTerm ctx term of
      Left e -> expectationFailure (show e)
      Right ty -> ty `shouldBe` expected


toMinimalSpec :: Spec
toMinimalSpec = describe "toMinimal test" $ do
  it "x" $ do
    let term = Variable "x"
        expected = Minimal.Variable "x"
    toMinimal term `shouldBe` expected

  it "f x" $ do
    let term = Application (#function @= Variable "f" <: #argument @= Variable "x" <: nil)
        expected = Minimal.Application (Minimal.Variable "f") (Minimal.Variable "x")
    toMinimal term `shouldBe` expected

  it "g (f x) y" $ do
    let term = Application (#function @= Application (#function @= Variable "g" <: #argument @= Application (#function @= Variable "f" <: #argument @= Variable "x" <: nil) <: nil) <: #argument @= Variable "y" <: nil)
        expected = Minimal.Application (Minimal.Application (Minimal.Variable "g") (Minimal.Application (Minimal.Variable "f") (Minimal.Variable "x"))) (Minimal.Variable "y")
    toMinimal term `shouldBe` expected
