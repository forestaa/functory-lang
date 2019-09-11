module Main where

import CLI.Graph
import qualified Data.ByteString.Lazy.Char8 as C
import RIO
import System.IO (stdout, stderr)

main :: IO ()
main = do
  C.hPutStrLn stderr "Functory: waiting for input..."
  content <- C.getContents
  forM_ (C.lines content) $ \input -> do
    result <- compile input
    C.hPutStrLn stdout result
    hFlush stdout
    C.hPutStrLn stderr $ mappend "Functory: write output: " result
