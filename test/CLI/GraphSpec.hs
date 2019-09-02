module CLI.GraphSpec where

import CLI.Graph
import Data.Aeson
import Data.Extensible
import RIO
import Test.Hspec

spec :: Spec
spec = do
  fromToJSONSpec

fromToJSONSpec :: Spec
fromToJSONSpec = do
  describe "fromJSON test" $ mapM_ (\(name, json, value) -> it name $ decode json `shouldBe` Just value) testcases
  describe "toJSON test"   $ mapM_ (\(name, json, value) -> it name $ encode value `shouldBe` json) testcases
  where
    testcases = [
          ("{ \"output\": { \"item\": 1, \"children\": [] }}", "{\"output\":{\"children\":[],\"item\":1}}", Graph (#output @= Vertex (#item @= 1 <: #children @= [] <: nil) <: nil))
        , ("{ \"output\": { \"item\": 1, \"children\": [{ \"item\": 2, \"target\": {\"item\":3, \"children\": []}}]}}", "{\"output\":{\"children\":[{\"item\":2,\"target\":{\"children\":[],\"item\":3}}],\"item\":1}}", Graph (#output @= Vertex (#item @= 1 <: #children @= [Edge (#item @= 2 <: #target @= Vertex (#item @= 3 <: #children @= [] <: nil) <: nil)] <: nil) <: nil))
      ]
