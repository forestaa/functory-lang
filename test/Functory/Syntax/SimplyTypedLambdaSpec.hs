module Functory.Syntax.SimplyTypedLambdaSpec (spec) where

import Data.Extensible
import Functory.Syntax.SimplyTypedLambda
import RIO
import qualified RIO.Vector as V
import Test.Hspec


spec :: Spec
spec = evalSpec

evalSpec :: Spec
evalSpec = describe "eval test" $ do
  it "x -> x" $ do
    let term = Constant "x"
        expected = term
        ctx = V.fromList $ [("x", ConstantBind Unit)]
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
    let term = Application $ #function @= Constant "f" <: #argument @= Constant "x" <: nil
        expected = term
        ctx = V.fromList [("f", ConstantBind (Arrow Unit Unit)), ("x", ConstantBind Unit)]
    case evalNamedTerm ctx term of
      Left e -> expectationFailure (show e)
      Right term' -> term' `shouldBe` expected

  it "(λx.x) x -> x" $ do
    let term = Application $ #function @= (Abstraction $ #name @= "x" <: #type @= Unit <: #body @= Variable "x" <: nil) <: #argument @= Constant "x" <: nil
        expected = Constant "x"
        ctx = V.fromList [("x", ConstantBind Unit)]
    case evalNamedTerm ctx term of
      Left e -> expectationFailure (show e)
      Right term' -> term' `shouldBe` expected

  it "(λx.λy. g x y) (f x) y -> g (f x) y" $ do
    let term = Application (#function @= Application (#function @= (Abstraction $ #name @= "x" <: #type @= Unit <: #body @= Abstraction (#name @= "y" <: #type @= Unit <: #body @= Application (#function @= Application (#function @= Constant "g" <: #argument @= Variable "x" <: nil) <: #argument @= Variable "y" <: nil) <: nil) <: nil) <: #argument @= Application (#function @= Constant "f" <: #argument @= Constant "x" <: nil) <: nil) <: #argument @= Constant "y" <: nil)
        expected = Application (#function @= Application (#function @= Constant "g" <: #argument @= Application (#function @= Constant "f" <: #argument @= Constant "x" <: nil) <: nil) <: #argument @= Constant "y" <: nil)
        ctx = V.fromList [("x", ConstantBind Unit), ("y", ConstantBind Unit), ("f", ConstantBind (Arrow Unit Unit)), ("g", ConstantBind (Arrow Unit (Arrow Unit Unit)))]
    case evalNamedTerm ctx term of
      Left e -> expectationFailure (show e)
      Right term' -> term' `shouldBe` expected
