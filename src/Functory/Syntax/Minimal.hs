module Functory.Syntax.Minimal where

import RIO (Show, Eq)
import SString

data Term = Variable SString | Application Term Term -- normalized term
  deriving (Show, Eq)
