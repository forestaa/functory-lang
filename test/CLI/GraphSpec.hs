module CLI.GraphSpec where

import CLI.Graph
import Data.Aeson
import Data.Extensible
import RIO
import Test.Hspec

spec :: Spec
spec = do
  fromToJSONSpec
  compileSpec

fromToJSONSpec :: Spec
fromToJSONSpec = do
  describe "fromJSON test" $ mapM_ (\(name, json, value) -> it name $ decode json `shouldBe` Just value) testcases
  describe "toJSON test"   $ mapM_ (\(name, json, value) -> it name $ encode value `shouldBe` json) testcases
  where
    testcases = [
          ("{ \"output\": { \"item\": 1, \"children\": [] }}", "{\"output\":{\"children\":[],\"item\":1}}", Graph (#output @= Vertex (#item @= 1 <: #children @= [] <: nil) <: nil))
        , ("{ \"output\": { \"item\": 1, \"children\": [{ \"item\": 2, \"target\": {\"item\":3, \"children\": []}}]}}", "{\"output\":{\"children\":[{\"item\":2,\"target\":{\"children\":[],\"item\":3}}],\"item\":1}}", Graph (#output @= Vertex (#item @= 1 <: #children @= [Edge (#item @= 2 <: #target @= Vertex (#item @= 3 <: #children @= [] <: nil) <: nil)] <: nil) <: nil))
      ]

compileSpec :: Spec
compileSpec = describe "compile test" $ do
  it "decode error" $ do
    let input = ""
        output = compile input
    case decode output of
      Nothing -> expectationFailure "failed to decode output"
      Just (CompileOutput r) -> r ^. #status `shouldBe` 1

  it "parse error" $ do
    let input = "{\"source\":\"\"}"
        output = compile input
    case decode output of
      Nothing -> expectationFailure "failed to decode output"
      Just (CompileOutput r) -> r ^. #status `shouldBe` 2

  it "callgraph error: missing constant" $ do
    let input = "{\"source\":\"fun hoge(x: (), y: ()) = let a: () = fuga(x) in pohe(a, y)\"}"
        output = compile input
    case decode output of
      Nothing -> expectationFailure "failed to decode output"
      Just (CompileOutput r) -> r ^. #status `shouldBe` 3

  it "callgraph success" $ do
    let input = "{\"source\":\"fun hot_water(wood: (), water: ()) = let energy: () = slash(wood) in fire(energy, water)\"}"
        output = compile input
    case decode output of
      Nothing -> expectationFailure "failed to decode output"
      Just (CompileOutput r) -> do
        r ^. #status `shouldBe` 0
        (fmap (\(CompileResult r) -> r ^. #name) (r ^. #result)) `shouldBe` Just "hot_water"
