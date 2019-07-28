module Functory.Syntax.SimplyTypedLambda where

import Control.Monad.Except
import Data.Extensible
import Data.Extensible.Effect.Default
import Functory.Syntax.NameLess
import RIO
import qualified RIO.Vector as V
import SString


data Type = 
    Unit
  | Arrow Type Type
  deriving (Eq)
data Term b = 
    Constant SString
  | Variable (Named b) 
  | Abstraction (Record '["name" :> SString, "type" :> Type, "body" :> Term b]) 
  | Application (Record '["function" :> Term b, "argument" :> Term b])
deriving instance (Eq (Named b)) => Eq (Term b)
type NamedTerm = Term 'True
type UnNamedTerm = Term 'False
instance Show Type where
  show Unit = "()"
  show (Arrow s t) = concat [show s, " -> ", show t]
instance Show (Named b) => Show (Term b) where
  show (Constant x) = show x
  show (Variable x) = show x
  show (Abstraction r) = concat ["λ", show (r ^. #name), ": ", show (r ^. #type), ". ", show (r ^. #body)]
  show (Application r) = concat ["(", show (r ^. #function), " ", show (r ^. #argument), ")"]

data TypedBinding = ConstantBind Type | VariableBind Type deriving (Show, Eq)
type VariableContext = Context TypedBinding
instance Binding Term TypedBinding where
  binding _ = VariableBind Unit             -- cannot use undefined in strict Haskell
instance FindVar Term 'True TypedBinding where
  findvar _ ctx x = V.findIndex isBound ctx
    where
      isBound (x', VariableBind _) = x == x'
      isBound _ = False
instance FindVar Term 'False TypedBinding where
  findvar _ ctx x = fst <$> ctx V.!? x
instance (FindVar Term a b, Binding Term b, MonadReader (Context b) m, MonadError (ContextError a b) m) => NameLess Term a b m where
  nameless (Constant x) = pure $ Constant x
  nameless (Variable x) = do
    ctx <- ask
    case findvar (Proxy :: Proxy Term) ctx x of
      Just x' -> pure $ Variable x'
      Nothing -> throwError $ MissingVariableInContext x ctx
  nameless (Abstraction r) = do
    let name = r ^. #name
    body <- local (V.cons (name, binding (Proxy :: Proxy Term))) $ nameless (r ^. #body)
    pure . Abstraction $ #name @= name <: #type @= r ^. #type <: #body @= body <: nil
  nameless (Application r) = do
    f <- nameless $ r ^. #function
    arg <- nameless $ r ^. #argument
    pure . Application $ #function @= f <: #argument @= arg <: nil


isVal :: UnNamedTerm -> Bool
isVal (Constant _)    = True
isVal (Abstraction _) = True
isVal (Application r) = case r ^. #function of
  Abstraction _ -> False
  _ -> isVal (r ^. #function) && isVal (r ^. #argument)
isVal _ = False

indexShift :: DeBrujinIndex -> UnNamedTerm -> UnNamedTerm
indexShift d = walk 0
  where
    walk _ (Constant x) = Constant x
    walk c (Variable n) | n < c     = Variable n
                        | otherwise = Variable $ n + d
    walk c (Abstraction r) = Abstraction $ over #body (walk (c+1)) r
    walk c (Application r) = Application . over #function (walk c) . over #argument (walk c) $ r
subst :: DeBrujinIndex -> UnNamedTerm -> UnNamedTerm -> UnNamedTerm
subst _ _ (Constant x) = Constant x
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
evalNamedTerm ctx = leaveEff . (`runReaderDef` ctx) . runEitherDef . ((mapLeftEff RestoreNameError . restoreName) <=< (fmap eval . mapLeftEff UnNameError . unName))


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

typing :: (MonadReader VariableContext m, MonadError TypingError m) => UnNamedTerm -> m Type
typing (Constant x) = do
  ctx <- ask
  case V.find ((==) x . fst) ctx of
    Just (_, ConstantBind ty) -> pure ty
    _ -> throwError . MissingDeclarationInVariableContext $ MissingVariableInContext x ctx
typing (Variable x) = do
  ctx <- ask
  case ctx V.!? x of
    Just (_, VariableBind ty) -> pure ty
    _ -> throwError . MissingVariableInVariableContext $ MissingVariableInContext x ctx
typing (Abstraction r) = do
  let domain = r ^. #type
  codomain <- local (V.cons (r ^. #name, VariableBind domain)) $ typing (r ^. #body)
  pure $ Arrow domain codomain
typing (Application r) = do
  fty <- typing $ r ^. #function
  argty <- typing $ r ^. #argument
  case fty of
    Arrow domain codomain 
      | domain == argty -> pure codomain
      | otherwise-> throwError $ NotMathcedTypeInApplication (r ^. #argument) argty domain
    _ -> throwError $ ArrowTypeExpected (r ^. #function) fty

data Errors = 
    NameLessError (NameLessErrors TypedBinding)
  | TypingError TypingError
  deriving (Eq)
instance Show Errors where
  show (NameLessError e) = concat ["NameLess Error: ", show e]
  show (TypingError e)   = concat ["Typing Error: ", show e]

typingNamedTerm :: VariableContext -> NamedTerm -> Either Errors Type
typingNamedTerm ctx = leaveEff . (`runReaderDef` ctx) . runEitherDef . ((mapLeftEff TypingError . typing) <=< (mapLeftEff (NameLessError . UnNameError) . unName))
