module Functory.Graph where

import Data.Extensible
import RIO.Set

newtype Node a = Node a
newtype Edge a = Edge (Record '["source" >: a, "target" >: a])
data Graph a = Graph (Record '["vertices" >: Set (Node a), "edges" >: Set (Edge a)])
type GraphWithOutput a = a -> Graph a
