module Functory.Syntax.Minimal where

import Data.Extensible
import RIO
import qualified RIO.Vector as V
import SString

-- Variable apeears here is a resource from external,
-- not equivalent to variables appearing in the programming language context
data Type = Unit | Arrow Type Type deriving (Eq, Ord, Show)
data Term = Variable SString | Application Term Term -- normalized term
  deriving (Show, Eq)

type VariableContext = Vector (SString, Type)
data TypingError =
    MissingVarialbe SString
  | TypeNotMatch Type Type
  deriving (Eq, Show)

typing :: (Lookup xs "contextMinimal" (ReaderEff VariableContext), Lookup xs "typingMinimal" (EitherEff TypingError)) => Term -> Eff xs Type
typing (Variable x) = do
  ctx <- askEff #contextMinimal
  case V.find ((==) x . fst) ctx of
    Just (_, ty) -> pure ty
    Nothing -> throwEff #typingMinimal $ MissingVarialbe x
typing (Application f x) = do
  fty <- typing f
  xty <- typing x
  case fty of
    Arrow domain codomain -> if xty == domain then pure codomain else throwEff #typingMinimal $ TypeNotMatch domain xty
    _ -> throwEff #typingMinimal $ TypeNotMatch (Arrow xty Unit) fty
