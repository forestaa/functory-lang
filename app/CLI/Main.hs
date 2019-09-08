module Main where

import CLI.Graph
import qualified Data.ByteString.Lazy.Char8 as C
import RIO

main :: IO ()
main = do
  C.putStrLn "Functory: waiting for input..."
  C.interact $ C.unlines . fmap compile . C.lines
