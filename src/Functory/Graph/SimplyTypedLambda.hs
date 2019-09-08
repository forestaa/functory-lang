module Functory.Graph.SimplyTypedLambda where

import Control.Monad.Except
import Data.Maybe
import qualified Data.Bifunctor as Bi
import Data.Extensible
import Functory.Graph
import qualified Functory.Graph.Minimal as Minimal
import qualified Functory.Syntax.Minimal as Minimal
import qualified Functory.Syntax.NameLess as NL
import qualified Functory.Syntax.SimplyTypedLambda as Lambda
import RIO
import qualified RIO.Map as Map
import qualified RIO.Vector as V

data ConvertError =
    TypeIsNotBase Lambda.NamedTerm Lambda.Type
  deriving (Show, Eq)

data CallGraphError =
    UnNameError (NL.UnNameError Lambda.TypedBinding)
  | RestoreNameError (NL.RestoreNameError Lambda.TypedBinding)
  | TypingMinimalError Minimal.TypingError
  | TypingSimpleError Lambda.TypingError
  | CallGraphMinimalError Minimal.ConvertError
  | CallGraphSimpleError ConvertError
  deriving (Show, Eq)
runCallGraph :: Minimal.VertexMap b
  -> Minimal.TypeItemMap a
  -> Minimal.VariableContext
  -> Lambda.VariableContext
  -> Eff '[
      "vertices" >: ReaderEff (Minimal.VertexMap b)
    , "typeItem" >: ReaderEff (Map.Map Minimal.Type a)
    , "contextMinimal" >: ReaderEff Minimal.VariableContext
    , "context" >: ReaderEff Lambda.VariableContext
    , "unName" >: EitherEff (NL.UnNameError Lambda.TypedBinding)
    , "restoreName" >: EitherEff (NL.RestoreNameError Lambda.TypedBinding)
    , "typingMinimal" >: EitherEff Minimal.TypingError
    , "typingSimple" >: EitherEff Lambda.TypingError
    , "callGraphMinimal" >: EitherEff Minimal.ConvertError
    , "callGraphSimple" >: EitherEff ConvertError
    ] c
  -> Either CallGraphError c
runCallGraph vertices items ctxMinimal ctx = join . join . join . join . join . leaveEff . runCallGraphSimple . runCallGraphMinimal . runTypingSimple . runTypingMinimal . runRestoreName . runUnName . runBindingContext . runContextMinimal . runTypeItem . runVertices
  where
    runVertices = flip (runReaderEff @"vertices") vertices
    runTypeItem = flip (runReaderEff @"typeItem") items
    runBindingContext = flip (runReaderEff @"context") ctx
    runContextMinimal = flip (runReaderEff @"contextMinimal") ctxMinimal
    runUnName = fmap (Bi.first UnNameError) . runEitherEff @"unName"
    runRestoreName = fmap (Bi.first RestoreNameError) . runEitherEff @"restoreName"
    runTypingMinimal = fmap (Bi.first TypingMinimalError) . runEitherEff @"typingMinimal"
    runTypingSimple = fmap (Bi.first TypingSimpleError) . runEitherEff @"typingSimple"
    runCallGraphMinimal = fmap (Bi.first CallGraphMinimalError) . runEitherEff @"callGraphMinimal"
    runCallGraphSimple = fmap (Bi.first CallGraphSimpleError) . runEitherEff @"callGraphSimple"


callGraph :: (
    Lookup xs "vertices" (ReaderEff (Minimal.VertexMap a))
  , Lookup xs "typeItem" (ReaderEff (Map.Map Minimal.Type a))
  , Lookup xs "context" (ReaderEff Lambda.VariableContext)
  , Lookup xs "unName"      (EitherEff (NL.UnNameError Lambda.TypedBinding))
  , Lookup xs "restoreName" (EitherEff (NL.RestoreNameError Lambda.TypedBinding))
  , Lookup xs "typingMinimal" (EitherEff Minimal.TypingError)
  , Lookup xs "typingSimple" (EitherEff Lambda.TypingError)
  , Lookup xs "contextMinimal" (ReaderEff Minimal.VariableContext)
  , Lookup xs "callGraphMinimal" (EitherEff Minimal.ConvertError)
  , Lookup xs "callGraphSimple"   (EitherEff ConvertError)
  , Ord a
  )
  => Lambda.NamedTerm
  -> Eff xs (Minimal.GraphWithOutput a a, Lambda.Type)
callGraph term = do
  ty <- Lambda.typing =<< NL.unName term
  items <- askEff #typeItem
  let (term', ctx) = Lambda.populateArgument term
  localEff #context ((V.++) (fmap g ctx)) . localEff #contextMinimal ((V.++) (fmap f ctx)) .
    localEff #vertices (\m -> V.foldr (\(x, Lambda.Unit) m -> maybe m (\b -> Map.insert x (Vertex b) m) $ items Map.!? Minimal.Unit) m ctx) $ do
      term'' <- NL.unName term'
      ty' <- Lambda.typing term''
      if Lambda.isBaseType ty'
        then fmap (, ty) . (Minimal.callGraph <=< (fmap Lambda.toMinimal . NL.restoreName)) $ Lambda.eval term''
        else throwEff #callGraphSimple $ TypeIsNotBase term' ty'
  where
    f (x, Lambda.Unit) = (x, Minimal.Unit)
    g (x, ty) = (x, Lambda.VariableBind ty)
