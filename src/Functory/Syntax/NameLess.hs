module Functory.Syntax.NameLess where

import Data.Extensible
import Data.Type.Bool
import RIO
import SString


type DeBrujinIndex = Int

type family Named (a :: Bool) = r | r -> a where
  Named 'True  = SString
  Named 'False = DeBrujinIndex
type NameLessVariable = Named 'False


class Binding (f :: Bool -> *) b where
  binding :: Proxy f -> b

type Context b = Vector (SString, b)
data ContextError a b = 
    MissingVariableInContext (Named a) (Context b)
deriving instance (Eq (Named a), Eq b) => Eq (ContextError a b)
instance (Show (Named a), Show b) => Show (ContextError a b) where
  show (MissingVariableInContext name ctx) = concat ["missing variable in context: variable: ", show name, ", Context: ", show ctx]

data NameLessErrors b =
    UnNameError (UnNameError b)
  | RestoreNameError (RestoreNameError b)
  deriving (Eq)
instance Show b => Show (NameLessErrors b) where
  show (UnNameError e) = "UnName Error: " ++ show e
  show (RestoreNameError e) = "RestoreName Error: " ++ show e


class FindVar (f :: Bool -> *) (a :: Bool) b where
  findvar :: Proxy f -> Context b -> Named a -> Maybe (Named (Not a))
class (FindVar f a b, Binding f b, Lookup xs "context" (ReaderEff (Context b)), Lookup xs k (EitherEff (ContextError a b))) => NameLess (f :: Bool -> *) a b k xs where
  nameless :: Proxy k -> f a -> Eff xs (f (Not a))
type UnNameError b = ContextError 'True b
unName :: NameLess f 'True b "unName" xs => f 'True -> Eff xs (f 'False)
unName = nameless (Proxy :: Proxy "unName")
type RestoreNameError b = ContextError 'False b
restoreName :: NameLess f 'False b "restoreName" xs => f 'False -> Eff xs (f 'True)
restoreName = nameless (Proxy :: Proxy "restoreName")
