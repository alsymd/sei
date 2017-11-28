{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( runParserLexeme
    , sexp
    , desugar
    , interp
    ) where
import Data.Maybe
import qualified Data.HashMap as M
import Control.Applicative
import Control.Monad.Identity
import Control.Monad
import Text.Parsec hiding ((<|>))
import qualified Text.Parsec.Token as P
import Data.ByteString.Char8


type Env = M.Map ByteString Value
type MyParsec = Parsec ByteString ()

data ExprS = NumS Integer
           | IdS ByteString
           | ExprS `Plus` ExprS
           | ExprS `Minus` ExprS
           | ExprS `Mult` ExprS
           | Neg ExprS
           | FdefS ByteString ExprS
           | AppS ExprS ExprS deriving(Show)


data ExprC = NumC Integer
           | IdC ByteString
           | ExprC :+: ExprC
           | ExprC :*: ExprC
           | FdefC ByteString ExprC
           | AppC ExprC ExprC deriving(Show)


data Value = NumV Integer
           | FdefV ByteString ExprC [Env] deriving(Show)

extChars = "!$%&*+-./:<=>?@^_~"


desugar :: ExprS -> ExprC
desugar s = case s of
  NumS i -> NumC i
  IdS s -> IdC s
  lhs `Plus` rhs -> desugar lhs :+: desugar rhs
  lhs `Mult` rhs -> desugar lhs :*: desugar rhs
  lhs `Minus` rhs -> desugar lhs :+:( NumC (-1) :*: desugar rhs)
  Neg i -> NumC (-1) :*: desugar i
  FdefS arg body -> FdefC arg (desugar body)
  AppS fun arg -> AppC (desugar fun) (desugar arg)
  

guiltDef :: P.GenLanguageDef ByteString () Identity
guiltDef = P.LanguageDef
  {P.commentStart = ""
  ,P.commentEnd = ""
  ,P.nestedComments = False
  ,P.commentLine = ";"
  ,P.identStart = letter <|> oneOf extChars
  ,P.identLetter = alphaNum <|> oneOf extChars
  ,P.opStart = undefined
  ,P.opLetter = undefined
  ,P.reservedNames = ["fun","+","-","*"]
  ,P.reservedOpNames = []
  ,P.caseSensitive = True}

lexer = P.makeTokenParser guiltDef

parens = P.parens lexer
identifier = P.identifier lexer
fun = P.reserved lexer "fun"
integer = P.integer lexer
plus = P.reserved lexer "+"
mult = P.reserved lexer "*"
minus = P.reserved lexer "-"
lexeme = P.lexeme lexer
whiteSpace = P.whiteSpace lexer
sexp :: MyParsec ExprS
sexp = parens (plusExpr <|> fdefExpr<|> minusExpr <|> multExpr <|> appExpr) <|>fmap NumS integer <|> fmap (IdS . pack) identifier
  where multExpr = mult *> pure Mult <*> sexp <*> sexp
        plusExpr = plus *> pure Plus <*> sexp <*> sexp
        fdefExpr = fun *> pure FdefS  <*> fmap pack identifier <*> sexp
        minusExpr = do
          minus
          lhs <- sexp
          (Minus lhs <$>  sexp) <|> pure (Neg lhs)
        appExpr = AppS <$> sexp <*> sexp
        

interp :: ExprC -> Value
interp  = flip interpEnv []


lookupValue id = Prelude.head . catMaybes . fmap (M.lookup id)

interpEnv :: ExprC -> [Env] -> Value
interpEnv exp env = case exp of
  NumC i -> NumV i
  i :+: j -> let NumV lhs = interpEnv i env
                 NumV rhs = interpEnv j env
             in NumV (lhs+rhs)
  i :*: j -> let NumV lhs = interpEnv i env
                 NumV rhs = interpEnv j env
             in NumV (lhs*rhs)
  AppC f arg -> let FdefV argId body fenv = interpEnv f env
                    argV = interpEnv arg env
                in interpEnv body ((M.singleton argId argV):env)
  IdC id -> lookupValue id $ env
  FdefC arg body -> let shadowedEnv = fmap (M.delete arg) env
                    in FdefV arg body shadowedEnv


runParserLexeme :: MyParsec a -> ByteString -> Either ParseError a
runParserLexeme p =  runParser (whiteSpace *> p) () ""
