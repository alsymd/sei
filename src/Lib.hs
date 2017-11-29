{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( runParserLexeme
    , sexp
    , desugar
    , interp
    , sexps
    ) where
import Control.Monad.ST
import Data.List
import Data.STRef
import Data.Maybe
import Data.List.NonEmpty
import qualified Data.HashMap as M
import Control.Applicative
import Control.Monad.Identity
import Control.Monad
import Text.Parsec hiding ((<|>))
import qualified Text.Parsec.Token as P
import Data.ByteString.Char8


type Env s = M.Map ByteString (Value s)
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
           deriving(Show)


data ExprC = NumC Integer
           | IdC ByteString
           | ExprC :+: ExprC
           | ExprC :*: ExprC
           | FdefC ByteString ExprC
           | AppC ExprC ExprC
           | BoxC ExprC
           | UnboxC ExprC
           | SetBoxC ExprC
           deriving(Show)


data Value s = NumV Integer
            | FdefV ByteString ExprC [Env s]
            | BoxV (STRef s (Value s)) 

instance Show (Value s) where
  show (BoxV _) = "#BoxV"
  show (FdefV arg expr _) = "(f " ++ unpack arg ++ " " ++ show expr ++ ")"
  show (NumV i) = show i
  

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
  ,P.reservedNames = ["fun","+","-","*","let","begin"]
  ,P.reservedOpNames = []
  ,P.caseSensitive = True}

lexer = P.makeTokenParser seiDef

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
sexp = ((<|>) <$> parens <*> brackets) (plusExpr <|> fdefExpr<|> minusExpr <|> multExpr <|> appExpr <|> letExprs <|> beginExprs) <|>fmap NumS integer <|> fmap (IdS . pack) identifier
  where multExpr = mult *> pure Mult <*> sexp <*> sexp
        plusExpr = plus *> pure Plus <*> sexp <*> sexp
        fdefExpr = fun *> pure FdefS  <*> fmap pack identifier <*> sexp
        minusExpr = do
          minus
          lhs <- sexp
          (Minus lhs <$>  sexp) <|> pure (Neg lhs)
        appExpr = AppS <$> sexp <*> sexp
        letExpr = ((<|>) <$> parens <*> brackets) (LetExpr <$> fmap pack identifier <*> sexp)
        letExprs = LetS <$> (letp *> ((<|>) <$> parens <*> brackets) (many1 letExpr)) <*> sexp
        beginExprs = beginp *> pure BeginS <*> many1 sexp
        

interp :: ExprC -> Value s
interp  = flip interpEnv []


lookupValue id = Prelude.head . catMaybes . fmap (M.lookup id)


-- interpEnvST :: runST $ do
  
sexps = Control.Applicative.many sexp

interpEnv :: ExprC -> [Env s] -> Value s
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
