module Functory.Syntax.Minimal where

import Data.Extensible
-- import RIO (Show, Eq)
import RIO
import qualified RIO.Vector as V
import SString

-- Variable apeears here is a resource from external,
-- not equivalent to variables appearing in the programming language context
data Type = Unit | Arrow Type Type deriving (Eq)
data Term = Variable SString | Application Term Term -- normalized term
  deriving (Show, Eq)

type VariableContext = Vector (SString, Type)
data TypingError =
    MissingVarialbe SString
  | TypeNotMatch Type Type

-- runTyping :: Lookup xs "context" (ReaderEff VariableContext) => Term -> Eff xs (Either TypingError Type)
-- runTyping ctx = leaveEff . runEitherEff @"typing" . flip (runReaderEff @"context") ctx . typing
-- runTyping = runEitherEff @"typing" . typing
typing :: (Lookup xs "context" (ReaderEff VariableContext), Lookup xs "typing" (EitherEff TypingError)) => Term -> Eff xs Type
typing (Variable x) = do
  ctx <- askEff #context
  case V.find ((==) x . fst) ctx of
    Just (_, ty) -> pure ty
    Nothing -> throwEff #typing $ MissingVarialbe x
typing (Application f x) = do
  fty <- typing f
  xty <- typing x
  case fty of
    Arrow domain codomain -> if xty == domain then pure codomain else throwEff #typing $ TypeNotMatch domain xty
    _ -> throwEff #typing $ TypeNotMatch (Arrow xty Unit) fty
