module Functory.Syntax.Minimal where

import RIO (String)

data Term = Variable String | Application Term Term -- normalized term
