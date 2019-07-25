module Functory.Syntax.NameLess where

import RIO
import qualified RIO.Map as Map

import Control.Monad.Except


import SString


type DeBrujinIndex = Int

type family Named (a :: Bool) = r | r -> a where
  Named 'True  = SString
  Named 'False = DeBrujinIndex
type NameLessVariable = Named 'False

type family Not (a :: Bool) where
  Not 'True = 'False
  Not 'False = 'True

class Binding (f :: Bool -> *) b where
  binding :: Proxy f -> b

type Context b = Map.Map SString b

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
class (FindVar f a b, Binding f b, MonadReader (Context b) m, MonadError (ContextError a b) m) => NameLess (f :: Bool -> *) a b m where
  nameless :: f a -> m (f (Not a))
type UnNameError b = ContextError 'True b
unName :: NameLess f 'True b m => f 'True -> m (f 'False)
unName = nameless
type RestoreNameError b = ContextError 'False b
restoreName :: NameLess f 'False b m => f 'False -> m (f 'True)
restoreName = nameless

-- leaveUnName :: NameLess f 'True b m => Context b -> f 'True -> Either (UnNameError b) (f 'False)
-- leaveUnName ctx t = leaveEff . (`runReaderDef` ctx) . runEitherDef $ unName t
-- leaveRestoreName :: NameLess f 'False b => Context b -> f 'False -> Either (RestoreNameError b) (f 'True)
-- leaveRestoreName ctx t = leaveEff . (`runReaderDef` ctx) . runEitherDef $ restoreName t
-- leaveUnRestoreName :: (NameLess f 'True b, NameLess f 'False b) => Context b -> f 'True -> Either (NameLessErrors b) (f 'True)
-- leaveUnRestoreName ctx t = leaveEff . (`runReaderDef` ctx) . runEitherDef $ do
--   t' <- mapLeftDef UnNameError $ unName t
--   mapLeftDef RestoreNameError $ restoreName t'
