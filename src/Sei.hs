{-# LANGUAGE OverloadedStrings #-}

module Sei
  ( runParserLexeme
  , sexp
  , desugar
  , interp
  , sexps
  ) where

import Control.Applicative
import Control.Category
import Control.Monad
import Control.Monad.Identity
import Control.Monad.ST
import Data.ByteString.Char8
import qualified Data.HashMap as M
import qualified Data.IntMap as IM
import Data.List
import Data.List.NonEmpty
import Data.Maybe
import Data.STRef
import Prelude hiding ((.), id)
import Text.Parsec hiding ((<|>))
import qualified Text.Parsec.Token as P

type Env = M.Map ByteString Int

type Storage = IM.IntMap Value

lookupLoc = M.lookup

fetchValue = IM.lookup

getValue k env sto = lookupLoc k env >>= flip fetchValue sto

type MyParsec = Parsec ByteString ()

data LetExpr =
  LetExpr ByteString
          ExprS
  deriving (Show)

data ExprS
  = NumS Integer
  | VarS ByteString
  | ExprS `Plus` ExprS
  | ExprS `Minus` ExprS
  | ExprS `Mult` ExprS
  | Neg ExprS
  | FdefS ByteString
          ExprS
  | AppS ExprS
         ExprS
  | LetS [LetExpr]
         ExprS
  | BeginS [ExprS]
  | SetS ByteString
         ExprS
  | LessS ExprS
          ExprS
          ExprS
          ExprS
  deriving (Show)

data ExprC
  = NumC Integer
  | VarC ByteString
  | ExprC :+: ExprC
  | ExprC :*: ExprC
  | FdefC ByteString
          ExprC
  | AppC ExprC
         ExprC
  | SeqC ExprC
         ExprC
  | SetC ByteString
         ExprC
  | LessC ExprC
          ExprC
          ExprC
          ExprC
  deriving (Show)

data Value
  = NumV Integer
  | FdefV ByteString
          ExprC
          Env
  deriving (Show)

extChars = "!$%&*+-./:<=>?@^_~"

desugar :: ExprS -> ExprC
desugar s =
  case s of
    NumS i -> NumC i
    VarS s -> VarC s
    lhs `Plus` rhs -> desugar lhs :+: desugar rhs
    lhs `Mult` rhs -> desugar lhs :*: desugar rhs
    lhs `Minus` rhs -> desugar lhs :+: (NumC (-1) :*: desugar rhs)
    Neg i -> NumC (-1) :*: desugar i
    FdefS arg body -> FdefC arg (desugar body)
    AppS fun arg -> AppC (desugar fun) (desugar arg)
    LetS letXs bodyExpr ->
      let f (LetExpr id body) exp = AppC (FdefC id exp) (desugar body)
          expC = desugar bodyExpr
      in Prelude.foldr f expC letXs
    BeginS beginXs -> Prelude.foldr1 SeqC $ fmap desugar beginXs
    SetS expr1 expr2 -> SetC expr1 (desugar expr2)
    LessS expr1 expr2 thenExpr elseExpr ->
      LessC
        (desugar expr1)
        (desugar expr2)
        (desugar thenExpr)
        (desugar elseExpr)

seiDef :: P.GenLanguageDef ByteString () Identity
seiDef =
  P.LanguageDef
  { P.commentStart = ""
  , P.commentEnd = ""
  , P.nestedComments = False
  , P.commentLine = ";"
  , P.identStart = letter <|> oneOf extChars
  , P.identLetter = alphaNum <|> oneOf extChars
  , P.opStart = undefined
  , P.opLetter = undefined
  , P.reservedNames = ["fun", "+", "-", "*", "let", "begin", "set!","blt"]
  , P.reservedOpNames = []
  , P.caseSensitive = True
  }

lexer = P.makeTokenParser seiDef

bltp = P.reserved lexer "blt"

setp = P.reserved lexer "set!"

beginp = P.reserved lexer "begin"

letp = P.reserved lexer "let"

parens = P.parens lexer

identifier = P.identifier lexer

fun = P.reserved lexer "fun"

integer = P.integer lexer

plus = P.reserved lexer "+"

mult = P.reserved lexer "*"

minus = P.reserved lexer "-"

lexeme = P.lexeme lexer

brackets = P.brackets lexer

whiteSpace = P.whiteSpace lexer

sexp :: MyParsec ExprS
sexp =
  ((<|>) <$> parens <*> brackets)
    (plusExpr <|> fdefExpr <|> minusExpr <|> multExpr <|> appExpr <|> letExprs <|>
     beginExprs <|>
     setExpr <|>
     bltExpr) <|>
  fmap NumS integer <|>
  fmap (VarS . pack) identifier
  where
    multExpr = mult *> pure Mult <*> sexp <*> sexp
    plusExpr = plus *> pure Plus <*> sexp <*> sexp
    fdefExpr = fun *> pure FdefS <*> fmap pack identifier <*> sexp
    minusExpr = do
      minus
      lhs <- sexp
      (Minus lhs <$> sexp) <|> pure (Neg lhs)
    appExpr = AppS <$> sexp <*> sexp
    letExpr =
      ((<|>) <$> parens <*> brackets)
        (LetExpr <$> fmap pack identifier <*> sexp)
    letExprs =
      LetS <$> (letp *> ((<|>) <$> parens <*> brackets) (many1 letExpr)) <*>
      sexp
    beginExprs = beginp *> pure BeginS <*> many1 sexp
    setExpr = setp *> pure SetS <*> fmap pack identifier <*> sexp
    bltExpr = bltp *> pure LessS <*> sexp <*> sexp <*> sexp <*> sexp

interp :: ExprC -> Value
interp expr = (\(x, _, _) -> x) $ interpEnvSto expr M.empty IM.empty [0 ..]

sexps = Control.Applicative.many sexp

interpEnvSto :: ExprC -> Env -> Storage -> [Int] -> (Value, Storage, [Int])
interpEnvSto exp env sto ids =
  case exp of
    NumC i -> (NumV i, sto, ids)
    i :+: j ->
      let (NumV (lhs), sto', ids') = interpEnvSto i env sto ids
          (NumV (rhs), sto'', ids'') = interpEnvSto j env sto' ids'
      in (NumV (lhs + rhs), sto'', ids'')
    i :*: j ->
      let (NumV lhs, sto', ids') = interpEnvSto i env sto ids
          (NumV rhs, sto'', ids'') = interpEnvSto j env sto' ids'
      in (NumV (lhs * rhs), sto'', ids'')
    AppC f arg ->
      let (FdefV argId body fenv, sto', ids') = interpEnvSto f env sto ids
          (argV, sto'', ids'') = interpEnvSto arg env sto' ids'
          (x:xs) = ids''
      in interpEnvSto body (M.insert argId x fenv) (IM.insert x argV sto'') xs
    VarC id -> (fromJust $ getValue id env sto, sto, ids)
    FdefC arg body ->
      let shadowedEnv = M.delete arg env
      in (FdefV arg body shadowedEnv, sto, ids)
    SeqC exp1 exp2 ->
      let (_, sto', ids') = interpEnvSto exp1 env sto ids
      in interpEnvSto exp2 env sto' ids'
    SetC var exp ->
      let loc = fromJust $ lookupLoc var env
          (v, sto', ids') = interpEnvSto exp env sto ids
      in (v, IM.insert loc v sto', ids')
    LessC expr1 expr2 thenExpr elseExpr ->
      let ((NumV v1), sto', ids') = interpEnvSto expr1 env sto ids
          ((NumV v2), sto'', ids'') = interpEnvSto expr2 env sto' ids'
      in case v1 < v2 of
           True -> interpEnvSto thenExpr env sto'' ids''
           False -> interpEnvSto elseExpr env sto'' ids''

runParserLexeme :: MyParsec a -> ByteString -> Either ParseError a
runParserLexeme p = runParser (whiteSpace *> p) () ""
