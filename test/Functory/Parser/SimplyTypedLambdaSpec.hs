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
typeParserSpec = describe "type parser test" $
  traverse_ (
    \(casename, text :: String, f, expected) ->
      it casename $ P.runParser f () "" text `shouldBe` Right expected
    ) [
        ("unit type", "()", unitType, Unit)
      , ("constant type", "Wood", constantType, Constant "Wood")
      , ("type expressoin: simgple arrow", "()->()", typeExp, Arrow Unit Unit)
      , ("type expression: right associative", "()->()->()", typeExp, Arrow Unit (Arrow Unit Unit))
      , ("type expressoin: simgple arrow with spaces", "() -> ()", typeExp, Arrow Unit Unit)
      , ("type expression: right associative with spaces", "() -> () -> ()", typeExp, Arrow Unit (Arrow Unit Unit))
      , ("type expression: right associative with spaces and constant", "Wood -> Fire -> Energy", typeExp, Arrow (Constant "Wood") (Arrow (Constant "Fire") (Constant "Energy")))
      ]

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

  it "let term: double" $ do
    let text = "let a: () -> () = f(x) in let b: () = x in a" :: String
    P.runParser termExp () "" text `shouldBe` Right (Application (#function @= Abstraction (#name @= "a" <: #type @= Arrow Unit Unit <: #body @= Application (#function @= Abstraction (#name @= "b" <: #type @= Unit <: #body @= Variable "a" <: nil) <: #argument @= Variable "x" <: nil) <: nil) <: #argument @= Application (#function @= Variable "f" <: #argument @= Variable "x" <: nil) <: nil))

  it "argument list in definition" $ do
    let text = "x: (), y: () -> (), z: () -> () -> ()" :: String
    P.runParser definitionArgumentList () "" text `shouldBe` Right [("x", Unit), ("y", Arrow Unit Unit), ("z", Arrow Unit (Arrow Unit Unit))]

  it "function definition" $ do
    let text = "fun hoge(x: (), y: ()) = let a: () = fuga(x) in pohe(a, y)" :: String
    P.runParser funcDefinition () "" text `shouldBe` Right ("hoge", Abstraction (#name @= "x" <: #type @= Unit <: #body @= Abstraction (#name @= "y" <: #type @= Unit <: #body @= Application (#function @= Abstraction (#name @= "a" <: #type @= Unit <: #body @= Application (#function @= Application (#function @= Variable "pohe" <: #argument @= Variable "a" <: nil) <: #argument @= Variable "y" <: nil) <: nil) <: #argument @= Application (#function @= Variable "fuga" <: #argument @= Variable "x" <: nil) <: nil) <: nil) <: nil), Application (#function @= Application (#function @= Abstraction (#name @= "x" <: #type @= Unit <: #body @= Abstraction (#name @= "y" <: #type @= Unit <: #body @= Application (#function @= Abstraction (#name @= "a" <: #type @= Unit <: #body @= Application (#function @= Application (#function @= Variable "pohe" <: #argument @= Variable "a" <: nil) <: #argument @= Variable "y" <: nil) <: nil) <: #argument @= Application (#function @= Variable "fuga" <: #argument @= Variable "x" <: nil) <: nil) <: nil) <: nil) <: #argument @= Variable "x" <: nil) <: #argument @= Variable "y" <: nil), [("x", Unit), ("y", Unit)])

  it "function definition with constant type" $ do
    let text = "fun hot_water(wood: Wood, water: Water) = let energy: Energy = slash(wood) in fire(energy, water)" :: String
    P.runParser funcDefinition () "" text `shouldBe` Right ("hot_water", Abstraction (#name @= "wood" <: #type @= Constant "Wood" <: #body @= Abstraction (#name @= "water" <: #type @= Constant "Water" <: #body @= Application (#function @= Abstraction (#name @= "energy" <: #type @= Constant "Energy" <: #body @= Application (#function @= Application (#function @= Variable "fire" <: #argument @= Variable "energy" <: nil) <: #argument @= Variable "water" <: nil) <: nil) <: #argument @= Application (#function @= Variable "slash" <: #argument @= Variable "wood" <: nil) <: nil) <: nil) <: nil), Application (#function @= Application (#function @= Abstraction (#name @= "wood" <: #type @= Constant "Wood" <: #body @= Abstraction (#name @= "water" <: #type @= Constant "Water" <: #body @= Application (#function @= Abstraction (#name @= "energy" <: #type @= Constant "Energy" <: #body @= Application (#function @= Application (#function @= Variable "fire" <: #argument @= Variable "energy" <: nil) <: #argument @= Variable "water" <: nil) <: nil) <: #argument @= Application (#function @= Variable "slash" <: #argument @= Variable "wood" <: nil) <: nil) <: nil) <: nil) <: #argument @= Variable "wood" <: nil) <: #argument @= Variable "water" <: nil), [("wood", Constant "Wood"), ("water", Constant "Water")])
