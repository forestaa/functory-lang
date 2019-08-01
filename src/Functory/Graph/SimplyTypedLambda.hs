module Functory.Graph.SimplyTypedLambda where

import Control.Monad.Except
import qualified Data.Bifunctor as Bi
import Data.Extensible
import qualified Functory.Graph.Minimal as Minimal
import qualified Functory.Syntax.NameLess as NL
import qualified Functory.Syntax.SimplyTypedLambda as Lambda
import RIO


data ConvertError =
    TypeIsNotUnit Lambda.Type
  deriving (Show, Eq)

data CallGraphError = 
    UnNameError (NL.UnNameError Lambda.TypedBinding)
  | RestoreNameError (NL.RestoreNameError Lambda.TypedBinding)
  | TypingError Lambda.TypingError
  | CallGraphMinimalError Minimal.ConvertError
  | CallGraphSimpleError ConvertError
  deriving (Show, Eq)
runCallGraph :: Minimal.VertexMap a 
  -> Lambda.VariableContext 
  -> Eff '[
      "vertices" >: State (Minimal.VertexMap a)
    , "context" >: ReaderEff Lambda.VariableContext
    , "unName" >: EitherEff (NL.UnNameError Lambda.TypedBinding)
    , "restoreName" >: EitherEff (NL.RestoreNameError Lambda.TypedBinding)
    , "typing" >: EitherEff Lambda.TypingError
    , "callGraphMinimal" >: EitherEff Minimal.ConvertError
    , "callGraphSimple" >: EitherEff ConvertError
    ] b 
  -> Either CallGraphError b
runCallGraph vertices ctx = join . join . join . join . leaveEff . runCallGraphSimple . runCallGraphMinimal . runTyping . runRestoreName . runUnName . flip (runReaderEff @"context") ctx . flip (evalStateEff @"vertices") vertices
  where
    runUnName = fmap (Bi.first UnNameError) . runEitherEff @"unName"
    runRestoreName = fmap (Bi.first RestoreNameError) . runEitherEff @"restoreName"
    runTyping = fmap (Bi.first TypingError) . runEitherEff @"typing"
    runCallGraphMinimal = fmap (Bi.first CallGraphMinimalError) . runEitherEff @"callGraphMinimal"
    runCallGraphSimple = fmap (Bi.first CallGraphSimpleError) . runEitherEff @"callGraphSimple"


callGraph :: (
    Lookup xs "vertices" (State (Minimal.VertexMap a))
  , Lookup xs "context" (ReaderEff Lambda.VariableContext)
  , Lookup xs "unName"      (EitherEff (NL.UnNameError Lambda.TypedBinding))
  , Lookup xs "restoreName" (EitherEff (NL.RestoreNameError Lambda.TypedBinding))
  , Lookup xs "typing"      (EitherEff Lambda.TypingError)
  , Lookup xs "callGraphMinimal" (EitherEff Minimal.ConvertError)
  , Lookup xs "callGraphSimple"   (EitherEff ConvertError)
  , Ord a
  ) 
  => Lambda.NamedTerm 
  -> Eff xs (Minimal.GraphWithOutput a)
callGraph term = do
  term' <- NL.unName term
  ty <- Lambda.typing term'
  if ty == Lambda.Unit
    then (Minimal.callGraph <=< (fmap Lambda.toMinimal . NL.restoreName)) $ Lambda.eval term'
    else throwEff #callGraphSimple $ TypeIsNotUnit ty
