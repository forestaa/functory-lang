module Functory.Syntax.Let where

import RIO (String)

-- TODO: fix semantics when the same name are used
data Term = Variable String | Application Term Term | Let String Term Term
