module CLI.GraphSpec where

import CLI.Graph
import Data.Aeson
import Data.Extensible
import RIO
import Test.Hspec

spec :: Spec
-- spec = pure ()
spec = do
  -- fromToJSONSpec
  compileSpec

-- fromToJSONSpec :: Spec
-- fromToJSONSpec = do
--   describe "fromJSON test" $ mapM_ (\(name, json, value) -> it name $ decode json `shouldBe` Just value) testcases
--   describe "toJSON test"   $ mapM_ (\(name, json, value) -> it name $ encode value `shouldBe` json) testcases
--   where
--     testcases = [
--           ("{ \"output\": { \"item\": 1, \"children\": [] }}", "{\"output\":{\"children\":[],\"item\":1}}", Graph (#output @= Vertex (#item @= 1 <: #children @= [] <: nil) <: nil))
--         , ("{ \"output\": { \"item\": 1, \"children\": [{ \"item\": 2, \"target\": {\"item\":3, \"children\": []}}]}}", "{\"output\":{\"children\":[{\"item\":2,\"target\":{\"children\":[],\"item\":3}}],\"item\":1}}", Graph (#output @= Vertex (#item @= 1 <: #children @= [Edge (#item @= 2 <: #target @= Vertex (#item @= 3 <: #children @= [] <: nil) <: nil)] <: nil) <: nil))
--       ]

compileSpec :: Spec
compileSpec = describe "compile test" $ do
  it "decode error" $ do
    let input = ""
        output = runIdentity $ compile input
    case decode output of
      Nothing -> expectationFailure "failed to decode output"
      Just (CompileResult r) -> r ^. #status `shouldBe` 1

  it "parse error" $ do
    let input = "{\"functory_id\":\"functory\",\"source\":\"\"}"
        output = runIdentity $ compile input
    case decode output of
      Nothing -> expectationFailure "failed to decode output"
      Just (CompileResult r) -> r ^. #status `shouldBe` 2

  it "callgraph error: missing constant" $ do
    let input = "{\"functory_id\":\"functory\",\"source\":\"fun hoge(x: (), y: ()) = let a: () = fuga(x) in pohe(a, y)\"}"
        output = runIdentity $ compile input
    case decode output of
      Nothing -> expectationFailure "failed to decode output"
      Just (CompileResult r) -> r ^. #status `shouldBe` 3

  -- it "callgraph success" $ do
  --   let input = "{\"functory_id\":\"functory\",\"source\":\"fun hot_water(wood: (), water: ()) = let energy: () = slash(wood) in fire(energy, water)\"}"
  --       output = runIdentity $ compile input
  --   case decode output of
  --     Nothing -> expectationFailure "failed to decode output"
  --     Just (CompileResult r) -> do
  --       traceShow (r ^. #description) $ pure ()
  --       r ^. #status `shouldBe` 0
  --       (fmap (\(CompileOutput r) -> r ^. #name) (r ^. #output)) `shouldBe` Just "hot_water"

  it "callgraph success with constant type" $ do
    let input = "{\"functory_id\":\"functory\",\"source\":\"fun hot_water(wood: Wood, water: Water) = let energy: Energy = slash(wood) in fire(energy, water)\"}"
        output = runIdentity $ compile input
    case decode output of
      Nothing -> expectationFailure "failed to decode output"
      Just (CompileResult r) -> do
        traceShow (r ^. #description) $ pure ()
        r ^. #status `shouldBe` 0
        (fmap (\(CompileOutput r) -> r ^. #name) (r ^. #output)) `shouldBe` Just "hot_water"
