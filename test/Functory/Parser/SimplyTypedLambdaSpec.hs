module Functory.Parser.SimplyTypedLambdaSpec where

import Data.Extensible
import Functory.Parser.SimplyTypedLambda
import Functory.Syntax.SimplyTypedLambda
import RIO
import Test.Hspec
import qualified Text.Parsec as P

spec :: Spec
spec = do
  typeParserSpec
  termParserSpec

typeParserSpec :: Spec
typeParserSpec = describe "type parser test" $ do
  it "unit type" $ do
    let text = "()" :: String
    P.runParser unitType () "" text `shouldBe` Right Unit
  it "type expressoin: simgple arrow" $ do
    let text = "()->()" :: String
    P.runParser typeExp () "" text `shouldBe` Right (Arrow Unit Unit)
  it "type expression: right associative" $ do
    let text = "()->()->()" :: String
    P.runParser typeExp () "" text `shouldBe` Right (Arrow Unit (Arrow Unit Unit))
  it "type expressoin: simgple arrow with spaces" $ do
    let text = "() -> ()" :: String
    P.runParser typeExp () "" text `shouldBe` Right (Arrow Unit Unit)
  it "type expression: right associative with spaces" $ do
    let text = "() -> () -> ()" :: String
    P.runParser typeExp () "" text `shouldBe` Right (Arrow Unit (Arrow Unit Unit))

termParserSpec :: Spec
termParserSpec = describe "term parser spec" $ do
  it "variable term" $ do
    let text = "x" :: String
    P.runParser termExp () "" text `shouldBe` Right (Variable "x")

  it "variable term: starts from let" $ do
    let text = "letter" :: String
    P.runParser termExp () "" text `shouldBe` Right (Variable "letter")

  it "argument list" $ do
    let text = "x,y,z" :: String
    P.runParser applicationArgumentList () "" text `shouldBe` Right [Variable "x", Variable "y", Variable "z"]

  it "application term" $ do
    let text = "f(x)" :: String
    P.runParser termExp () "" text `shouldBe` Right (Application (#function @= Variable "f" <: #argument @= Variable "x" <: nil))

  it "application term: several arguments" $ do
    let text = "f(x,y)" :: String
    P.runParser termExp () "" text `shouldBe` Right (Application (#function @= Application (#function @= Variable "f" <: #argument @= Variable "x" <: nil) <: #argument @= Variable "y" <: nil))

  it "let term" $ do
    let text = "let a: () = x in a" :: String
    P.runParser termExp () "" text `shouldBe` Right (Application (#function @= Abstraction (#name @= "a" <: #type @= Unit <: #body @= Variable "a" <: nil) <: #argument @= Variable "x" <: nil))

  it "let term: doulbe" $ do
    let text = "let a: () -> () = f(x) in let b: () = x in a" :: String
    P.runParser termExp () "" text `shouldBe` Right (Application (#function @= Abstraction (#name @= "a" <: #type @= Arrow Unit Unit <: #body @= Application (#function @= Abstraction (#name @= "b" <: #type @= Unit <: #body @= Variable "a" <: nil) <: #argument @= Variable "x" <: nil) <: nil) <: #argument @= Application (#function @= Variable "f" <: #argument @= Variable "x" <: nil) <: nil))

  it "argument list in definition" $ do
    let text = "x: (), y: () -> (), z: () -> () -> ()" :: String
    P.runParser definitionArgumentList () "" text `shouldBe` Right [("x", Unit), ("y", Arrow Unit Unit), ("z", Arrow Unit (Arrow Unit Unit))]

  it "function definition" $ do
    let text = "fun hoge(x: (), y: ()) = let a: () = fuga(x) in pohe(a, y)" :: String
    P.runParser funcDefinition () "" text `shouldBe` Right ("hoge", Abstraction (#name @= "x" <: #type @= Unit <: #body @= Abstraction (#name @= "y" <: #type @= Unit <: #body @= Application (#function @= Abstraction (#name @= "a" <: #type @= Unit <: #body @= Application (#function @= Application (#function @= Variable "pohe" <: #argument @= Variable "a" <: nil) <: #argument @= Variable "y" <: nil) <: nil) <: #argument @= Application (#function @= Variable "fuga" <: #argument @= Variable "x" <: nil) <: nil) <: nil) <: nil))
