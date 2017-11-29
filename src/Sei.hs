{-# LANGUAGE OverloadedStrings #-}
module Sei
    ( runParserLexeme
    , sexp
    , desugar
    , interp
    , sexps
    ) where
import qualified Data.IntMap as IM
import Control.Monad.ST
import Data.List
import Data.STRef
import Control.Category
import Prelude hiding ((.),id)
import Data.Maybe
import Data.List.NonEmpty
import qualified Data.HashMap as M
import Control.Applicative
import Control.Monad.Identity
import Control.Monad
import Text.Parsec hiding ((<|>))
import qualified Text.Parsec.Token as P
import Data.ByteString.Char8


type Env = M.Map ByteString Int
type Storage = IM.IntMap Value
lookupLoc = M.lookup
fetchValue = IM.lookup
getValue k env sto = lookupLoc k env >>= flip fetchValue sto

type MyParsec = Parsec ByteString ()

data LetExpr = LetExpr ByteString ExprS deriving(Show)

data ExprS = NumS Integer
           | IdS ByteString
           | ExprS `Plus` ExprS
           | ExprS `Minus` ExprS
           | ExprS `Mult` ExprS
           | Neg ExprS
           | FdefS ByteString ExprS
           | AppS ExprS ExprS
           | LetS [LetExpr] ExprS
           | BeginS [ExprS]
           | BoxS ExprS
           | UnboxS ExprS
           | SetBoxS ExprS ExprS
           deriving(Show)


data ExprC = NumC Integer
           | IdC ByteString
           | ExprC :+: ExprC
           | ExprC :*: ExprC
           | FdefC ByteString ExprC
           | AppC ExprC ExprC
           | BoxC ExprC
           | UnboxC ExprC
           | SetBoxC ExprC ExprC
           | SeqC ExprC ExprC
           deriving(Show)


data Value  = NumV Integer
            | FdefV ByteString ExprC Env
            | BoxV Int deriving(Show)


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
  LetS letXs bodyExpr ->
    let f (LetExpr id body) exp = AppC (FdefC id exp ) (desugar body)
        expC = desugar bodyExpr
    in Prelude.foldr f expC letXs
  BeginS beginXs ->
     let Just (bodyExpr,restRev) =  Data.List.uncons (Data.List.reverse beginXs)
     in desugar $ LetS (fmap (LetExpr "") (Prelude.reverse restRev)) bodyExpr
  BoxS expr -> BoxC (desugar expr)
  UnboxS expr -> UnboxC (desugar expr)
  SetBoxS exp exp' -> SetBoxC (desugar exp) (desugar exp')
  
  

seiDef :: P.GenLanguageDef ByteString () Identity
seiDef = P.LanguageDef
  {P.commentStart = ""
  ,P.commentEnd = ""
  ,P.nestedComments = False
  ,P.commentLine = ";"
  ,P.identStart = letter <|> oneOf extChars
  ,P.identLetter = alphaNum <|> oneOf extChars
  ,P.opStart = undefined
  ,P.opLetter = undefined
  ,P.reservedNames = ["fun","+","-","*","let","begin","box","unbox","set-box!"]
  ,P.reservedOpNames = []
  ,P.caseSensitive = True}

lexer = P.makeTokenParser seiDef

boxp = P.reserved lexer "box"
unboxp = P.reserved lexer "unbox"
setboxp = P.reserved lexer "set-box!"
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
sexp = ((<|>) <$> parens <*> brackets) (plusExpr <|> fdefExpr<|> minusExpr <|> multExpr <|> appExpr <|> letExprs <|> beginExprs <|> boxExpr <|> unboxExpr <|> setboxExpr) <|>fmap NumS integer <|> fmap (IdS . pack) identifier
  where multExpr = mult *> pure Mult <*> sexp <*> sexp
        plusExpr = plus *> pure Plus <*> sexp <*> sexp
        fdefExpr = fun *> pure FdefS  <*> fmap pack identifier <*> sexp
        boxExpr = boxp *> pure BoxS <*> sexp
        unboxExpr = unboxp *> pure UnboxS <*> sexp
        setboxExpr = setboxp *> pure SetBoxS <*> sexp <*> sexp
        minusExpr = do
          minus
          lhs <- sexp
          (Minus lhs <$>  sexp) <|> pure (Neg lhs)
        appExpr = AppS <$> sexp <*> sexp
        letExpr = ((<|>) <$> parens <*> brackets) (LetExpr <$> fmap pack identifier <*> sexp)
        letExprs = LetS <$> (letp *> ((<|>) <$> parens <*> brackets) (many1 letExpr)) <*> sexp
        beginExprs = beginp *> pure BeginS <*> many1 sexp
        

interp :: ExprC -> Value
interp expr  = (\(x,_,_) -> x) $ interpEnvSto expr M.empty IM.empty [0..]



  
sexps = Control.Applicative.many sexp

interpEnvSto :: ExprC -> Env -> Storage -> [Int] -> (Value,Storage,[Int])
interpEnvSto exp env sto ids = case exp of
  NumC i -> (NumV i,sto,ids)
  i :+: j -> let (NumV (lhs),sto',ids') = interpEnvSto i env sto ids
                 (NumV (rhs),sto'',ids'') = interpEnvSto j env sto' ids'
             in (NumV (lhs+rhs),sto'',ids'')
  i :*: j -> let (NumV lhs,sto',ids') = interpEnvSto i env sto ids
                 (NumV rhs,sto'',ids'') = interpEnvSto j env sto' ids'
             in (NumV (lhs*rhs),sto'',ids'')
  AppC f arg -> let (FdefV argId body fenv, sto',ids') = interpEnvSto f env sto ids
                    (argV,sto'',ids'') = interpEnvSto arg env sto' ids'
                    (x:xs) = ids''
                in interpEnvSto body (M.insert argId x env) (IM.insert x argV sto'') xs
  IdC id -> (fromJust $ getValue id env sto, sto,ids)
  FdefC arg body -> let shadowedEnv = M.delete arg env
                    in (FdefV arg body shadowedEnv,sto,ids)
  SeqC exp1 exp2 -> let (_,sto',ids') = interpEnvSto exp1 env sto ids
                    in interpEnvSto exp2 env sto' ids'
  BoxC exp -> let (v,sto',ids') = interpEnvSto exp env sto ids
                  (x:ids'') = ids'
              in (BoxV x,IM.insert x v sto',ids'')
  UnboxC exp -> let (BoxV v, sto', ids') = interpEnvSto exp env sto ids
                in (fromJust $ fetchValue v sto', sto',ids')
  SetBoxC box exp -> let (BoxV loc, sto', ids') = interpEnvSto box env sto ids
                         (v, sto'', ids'') = interpEnvSto exp env sto' ids'
                     in (v,IM.insert loc v sto'',ids'')

runParserLexeme :: MyParsec a -> ByteString -> Either ParseError a
runParserLexeme p =  runParser (whiteSpace *> p) () ""