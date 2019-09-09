module Functory.Syntax.SimplyTypedLambda where

import Control.Monad.Except
import Data.Aeson
import qualified Data.Bifunctor as Bi
import Data.Extensible
import qualified Functory.Syntax.Minimal as Minimal
import Functory.Syntax.NameLess
import RIO
import qualified RIO.Vector as V
import SString


data Type =
    Unit
  | Arrow Type Type
  deriving (Eq, Generic)
data Term b =
    Variable (Named b)
  | Abstraction (Record '["name" :> SString, "type" :> Type, "body" :> Term b])
  | Application (Record '["function" :> Term b, "argument" :> Term b])
deriving instance (Eq (Named b)) => Eq (Term b)
type NamedTerm = Term 'True
type UnNamedTerm = Term 'False
instance Show Type where
  show Unit = "()"
  show (Arrow s t) = concat ["(", show s, " -> ", show t, ")"]
instance Show (Named b) => Show (Term b) where
  show (Variable x) = show x
  show (Abstraction r) = concat ["(λ", show (r ^. #name), ": ", show (r ^. #type), ". ", show (r ^. #body), ")"]
  show (Application r) = concat ["(", show (r ^. #function), " ", show (r ^. #argument), ")"]
instance ToJSON Type where
  toJSON = String . utf8BuilderToText . displayShow
instance FromJSON Type where
  parseJSON = withText "Type" $ \s -> pure Unit

data TypedBinding = VariableBind Type deriving (Show, Eq)
type VariableContext = Context TypedBinding
instance Binding Term TypedBinding where
  binding _ = VariableBind Unit             -- cannot use undefined in strict Haskell
instance FindVar Term 'True TypedBinding where
  findvar _ ctx x = V.findIndex isBound ctx
    where
      isBound (x', VariableBind _) = x == x'
instance FindVar Term 'False TypedBinding where
  findvar _ ctx x = fst <$> ctx V.!? x
instance (FindVar Term a b, Binding Term b, Lookup xs "context" (ReaderEff (Context b)), Lookup xs k (EitherEff (ContextError a b))) => NameLess Term a b k xs where
  nameless k (Variable x) = do
    ctx <- askEff #context
    case findvar (Proxy :: Proxy Term) ctx x of
      Just x' -> pure $ Variable x'
      Nothing -> throwEff k $ MissingVariableInContext x ctx
  nameless k (Abstraction r) = do
    let name = r ^. #name
    body <- localEff #context (V.cons (name, binding (Proxy :: Proxy Term))) $ nameless k (r ^. #body)
    pure . Abstraction $ #name @= name <: #type @= r ^. #type <: #body @= body <: nil
  nameless k (Application r) = do
    f <- nameless k $ r ^. #function
    arg <- nameless k $ r ^. #argument
    pure . Application $ #function @= f <: #argument @= arg <: nil


isVal :: UnNamedTerm -> Bool
isVal (Variable _) = True
isVal (Abstraction _) = True
isVal (Application r) = case r ^. #function of
  Abstraction _ -> False
  _ -> isVal (r ^. #function) && isVal (r ^. #argument)
isVal _ = False

indexShift :: DeBrujinIndex -> UnNamedTerm -> UnNamedTerm
indexShift d = walk 0
  where
    walk c (Variable n) | n < c     = Variable n
                        | otherwise = Variable $ n + d
    walk c (Abstraction r) = Abstraction $ over #body (walk (c+1)) r
    walk c (Application r) = Application . over #function (walk c) . over #argument (walk c) $ r
subst :: DeBrujinIndex -> UnNamedTerm -> UnNamedTerm -> UnNamedTerm
subst j s (Variable n) | n == j    = s
                       | otherwise = Variable n
subst j s (Abstraction r) = Abstraction $ over #body (subst (j+1) (indexShift 1 s)) r
subst j s (Application r) = Application . over #function (subst j s) . over #argument (subst j s) $ r

betaReduction :: UnNamedTerm -> UnNamedTerm -> UnNamedTerm
betaReduction s t = indexShift (-1) $ subst 0 (indexShift 1 s) t

evalByOneStep :: UnNamedTerm -> Maybe UnNamedTerm
evalByOneStep (Application r) = case r ^. #function of
  Abstraction r' | isVal (r ^. #argument) -> Just $ betaReduction (r ^. #argument) (r' ^. #body)
  v | isVal v -> Application . flip (set #argument) r <$> evalByOneStep (r ^. #argument)
  t -> Application . flip (set #function) r <$> evalByOneStep t
evalByOneStep _ = Nothing

eval :: UnNamedTerm -> UnNamedTerm
eval t = maybe t eval $ evalByOneStep t

evalNamedTerm :: VariableContext -> NamedTerm -> Either (NameLessErrors TypedBinding) NamedTerm
evalNamedTerm ctx = join . leaveEff . runUnName . runRestoreName . flip (runReaderEff @"context") ctx . (restoreName <=< (fmap eval . unName))
  where
    runRestoreName = fmap (Bi.first RestoreNameError) . runEitherEff @"restoreName"
    runUnName = fmap (Bi.first UnNameError) . runEitherEff @"unName"


data TypingError =
    MissingDeclarationInVariableContext (ContextError 'True TypedBinding)
  | MissingVariableInVariableContext (ContextError 'False TypedBinding)
  | NotMathcedTypeInApplication UnNamedTerm Type Type
  | ArrowTypeExpected UnNamedTerm Type
  deriving Eq
instance Show TypingError where
  show (MissingDeclarationInVariableContext e) = concat ["Missing constant declaration: ", show e]
  show (MissingVariableInVariableContext e) = show e
  show (NotMathcedTypeInApplication term got expected) = concat ["Couldn't match type: ", show expected, " expected, Actual type = ", show got, " of ", show term]
  show (ArrowTypeExpected term got) = concat ["Couldn't match type: Arrow type expected, Actual type = ", show got, " of ", show term]

isBaseType :: Type -> Bool
isBaseType Unit = True
isBaseType _ = False

typing :: (Lookup xs "context" (ReaderEff VariableContext), Lookup xs "typingSimple" (EitherEff TypingError)) => UnNamedTerm -> Eff xs Type
typing (Variable x) = do
  ctx <- askEff #context
  case ctx V.!? x of
    Just (_, VariableBind ty) -> pure ty
    _ -> throwEff #typingSimple . MissingVariableInVariableContext $ MissingVariableInContext x ctx
typing (Abstraction r) = do
  let domain = r ^. #type
  codomain <- localEff #context (V.cons (r ^. #name, VariableBind domain)) $ typing (r ^. #body)
  pure $ Arrow domain codomain
typing (Application r) = do
  fty <- typing $ r ^. #function
  argty <- typing $ r ^. #argument
  case fty of
    Arrow domain codomain
      | domain == argty -> pure codomain
      | otherwise-> throwEff #typingSimple $ NotMathcedTypeInApplication (r ^. #argument) argty domain
    _ -> throwEff #typingSimple $ ArrowTypeExpected (r ^. #function) fty

data Errors =
    NameLessError (NameLessErrors TypedBinding)
  | TypingError TypingError
  deriving (Eq)
instance Show Errors where
  show (NameLessError e) = concat ["NameLess Error: ", show e]
  show (TypingError e)   = concat ["Typing Error: ", show e]

typingNamedTerm :: VariableContext -> NamedTerm -> Either Errors Type
typingNamedTerm ctx = join . leaveEff . flip (runReaderEff @"context") ctx . runTyping . runUnName . (typing <=< unName)
  where
    runUnName = fmap (Bi.first (NameLessError . UnNameError)) . runEitherEff @"unName"
    runTyping = fmap (Bi.first TypingError) . runEitherEff @"typingSimple"


toMinimal :: NamedTerm -> Minimal.Term
toMinimal (Variable x) = Minimal.Variable x
toMinimal (Application r) = Minimal.Application (toMinimal $ r ^. #function) (toMinimal $ r ^. #argument)

populateArgument :: NamedTerm -> (NamedTerm, Vector (SString, Type))
populateArgument = leaveEff . flip (evalStateEff @"unique") 0 . populateArgumentInternal
populateArgumentInternal :: Lookup xs "unique" (State Int) => NamedTerm -> Eff xs (NamedTerm, Vector (SString, Type))
populateArgumentInternal (Abstraction r) = do
  name <- getsEff #unique $ pack . show
  modifyEff #unique $ (+) 1
  (body, ctx) <- populateArgumentInternal $ r ^. #body
  pure $ (Application $ #function @= (Abstraction $ set #body body r) <: #argument @= Variable name <: nil, V.cons (name, r ^. #type) ctx)
populateArgumentInternal x = pure (x, V.empty)
