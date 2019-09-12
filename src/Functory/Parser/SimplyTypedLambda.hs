module Functory.Parser.SimplyTypedLambda where


import           Control.Applicative
import           Data.Extensible
import           Functory.Syntax.SimplyTypedLambda
import           RIO
import           SString
import qualified Text.Parsec as P
import           Text.Parsec (ParsecT, Stream)
import           Text.Parsec.Expr
import qualified Text.Parsec.Token as P
import           Text.Parsec.Token (GenLanguageDef(..))

functoryLang :: Stream s m Char => GenLanguageDef s u m
functoryLang = LanguageDef
  { commentStart = ""
  , commentEnd = ""
  , commentLine = "%"
  , nestedComments = False
  , identStart = P.lower <|> P.oneOf "_"
  , identLetter = P.alphaNum <|> P.oneOf "_"
  , opStart = P.oneOf "+-*/"
  , opLetter = P.oneOf "+-*/"
  , reservedNames = ["fun", ":", "->", "let", "in"]
  , reservedOpNames = ["->"]
  , caseSensitive = True
  }

lexer :: Stream s m Char => P.GenTokenParser s u m
lexer = P.makeTokenParser functoryLang

identifier :: Stream s m Char => ParsecT s u m String
identifier = P.identifier lexer

symbol :: Stream s m Char => String -> ParsecT s u m String
symbol = P.symbol lexer

reserved :: Stream s m Char => String -> ParsecT s u m ()
reserved = P.reserved lexer

reservedOp ::Stream s m Char => String -> ParsecT s u m ()
reservedOp = P.reservedOp lexer


typeExp ::Stream s m Char => ParsecT s u m Type
typeExp = buildExpressionParser typeExprTable typeTerm
  where
    typeTerm = baseType
    typeExprTable = [
        [Infix arrowType AssocRight]
      ]

unitType :: Stream s m Char => ParsecT s u m Type
unitType = Unit <$ symbol "()"

constantType :: Stream s m Char => ParsecT s u m Type
constantType = do
  x <- P.upper
  xs <- many (identLetter functoryLang)
  P.spaces
  pure . Constant $ pack (x:xs)

baseType :: Stream s m Char => ParsecT s u m Type
baseType = P.choice [
    unitType
  , constantType
  ]



arrowType :: Stream s m Char => ParsecT s u m (Type -> Type -> Type)
arrowType =  reservedOp "->" $> Arrow

termExp :: Stream s m Char => ParsecT s u m NamedTerm
termExp = P.choice [
      letTerm
    , P.try applicationTerm
    , variableTerm
  ]

variableTerm :: Stream s m Char => ParsecT s u m NamedTerm
variableTerm = Variable . pack <$> identifier

applicationTerm :: Stream s m Char => ParsecT s u m NamedTerm
applicationTerm = do
  f <- pack <$> identifier
  args <- P.between (symbol "(") (symbol ")") applicationArgumentList
  pure $ foldl' (\t1 t2 -> Application (#function @= t1 <: #argument @= t2 <: nil)) (Variable f) args

applicationArgumentList :: Stream s m Char => ParsecT s u m [NamedTerm]
applicationArgumentList = termExp `P.sepBy` (symbol ",")

identifierWithType :: Stream s m Char => ParsecT s u m (SString, Type)
identifierWithType = (,) <$> (pack <$> identifier) <* symbol ":" <*> typeExp

letTerm :: Stream s m Char => ParsecT s u m NamedTerm
letTerm = do
  reserved "let"
  (x, ty) <- identifierWithType
  reserved "="
  t <- termExp
  reserved "in"
  body <- termExp
  pure $ Application (#function @= Abstraction (#name @= x <: #type @= ty <: #body @= body <: nil) <: #argument @= t <: nil)

definitionArgumentList :: Stream s m Char => ParsecT s u m [(SString, Type)]
definitionArgumentList = identifierWithType `P.sepBy` symbol ","

funcDefinition :: Stream s m Char => ParsecT s u m (String, NamedTerm, NamedTerm, [(SString, Type)])
funcDefinition = do
  reserved "fun"
  f <- identifier
  args <- P.between (symbol "(") (symbol ")") definitionArgumentList
  reserved "="
  body <- termExp
  let rowTerm = foldr (\(x, ty) term -> Abstraction (#name @= x <: #type @= ty <: #body @= term <: nil)) body args
      populatedTerm = foldl' (\term (x, _) -> Application (#function @= term <: #argument @= Variable x <: nil)) rowTerm args
  pure (f, rowTerm, populatedTerm, args)
