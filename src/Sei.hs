{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall #-}
{-# LANGUAGE RecursiveDo #-}
module Sei
  ( runParserLexeme
  , sexp
  , desugar
  , sexps
  , interp'
  ) where
import Control.Applicative
import Debug.Trace
import Control.Category
import Control.Monad
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Except
import Data.ByteString.Char8
import qualified Data.HashMap as M
import qualified Data.IntMap as IM
import Data.List
import Data.Maybe
import Prelude hiding ((.), id)
import Text.Parsec hiding ((<|>))
import qualified Text.Parsec.Token as P
import Control.Lens

type InterpMonad = (StateT InterpState (ExceptT String Identity))

runInterp :: StateT a1 (ExceptT e Identity) a -> a1 -> Either e (a, a1)
runInterp s = runIdentity . runExceptT . runStateT s

type Env = M.Map ByteString Int

data LetExpr =
  LetExpr ByteString
          ExprS
  deriving (Show)


data Value
  = NumV Integer
  | FdefV ByteString
          ExprC
          Env
  | BoolV Bool
  | VoidV
  deriving (Show)

isNumV :: Value -> Bool
isNumV (NumV _) = True
isNumV _ = False

extractNumV :: Value -> InterpMonad Integer
extractNumV (NumV i) = pure i
extractNumV v = throwError $ "Expecting NumV but encountered " ++  show v

extractBoolV :: Value -> InterpMonad Bool
extractBoolV (BoolV b) = pure b
extractBoolV v = throwError $ "Expecting NumV but encountered " ++ show v

extractFdefV :: Value -> InterpMonad Value
extractFdefV f@(FdefV _ _ _) = pure f
extractFdefV v = throwError $ "Expecting FdefV but encountered " ++ show v

isBoolV :: Value -> Bool
isBoolV (BoolV _) = True
isBoolV _ = False


data ExprS
  = NumS Integer
  | VarS ByteString
  | ExprS `Plus` ExprS
  | ExprS `Minus` ExprS
  | ExprS `Mult` ExprS
  | Neg ExprS
  | FdefS [ByteString]
          ExprS
  | AppS ExprS
         [ExprS]
  | LetS [LetExpr]
         ExprS
  | BeginS [ExprS]
  | SetS ByteString
         ExprS
  | LessS ExprS
          ExprS
          ExprS
          ExprS
  | LetRecS [LetExpr]
          ExprS
  | BoolS Bool
  | IfS ExprS ExprS ExprS
  | DefS ByteString ExprS
  | VoidS
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
  | RecAppC ExprC ExprC
  | BoolC Bool
  | IfC ExprC ExprC ExprC
  | VoidC
  deriving (Show)

type Storage = IM.IntMap Value

data InterpState = InterpState {_env :: Env, _sto :: Storage, _ids :: [Int]} deriving (Show)
emptyState :: InterpState
emptyState = InterpState{_env=M.empty, _sto=IM.empty, _ids=[0..]}
makeLenses ''InterpState

newLoc :: InterpMonad Int
newLoc = do (x:xs) <- fmap (^.ids) get
            modify (ids .~ xs)
            pure x

recycleLoc :: Int -> InterpMonad ()
recycleLoc x = modify ((over ids  (x:)) :: InterpState -> InterpState)

lookupLoc :: ByteString -> M.Map ByteString a -> Maybe a
lookupLoc = M.lookup

fetchValue :: IM.Key -> IM.IntMap a -> Maybe a
fetchValue = IM.lookup

getValue :: ByteString -> M.Map ByteString IM.Key -> IM.IntMap b -> Maybe b
getValue k env sto = lookupLoc k env >>= flip fetchValue sto


storeV :: Value -> InterpMonad Int
storeV v = do
  loc <- newLoc
  modify (over sto (IM.insert loc v))
  pure loc

fetchV :: ByteString -> InterpMonad Value
fetchV id = do
  loc <- fetchLoc id
  mv <- gets (IM.lookup loc . view sto)
  case mv of
    Nothing -> throwError $ "Variable " ++ show id ++ " exists but storage is not available. Please report this as a bug."
    Just v -> pure v

modifyV :: Int -> Value -> InterpMonad Value
modifyV loc v = modify (over sto (IM.insert loc v)) *> pure v

fetchLoc :: ByteString -> InterpMonad Int
fetchLoc id = do
  mloc <- gets (M.lookup id . view env)
  case mloc of
    Nothing -> throwError $ "No variable named " ++ unpack id
    Just loc -> pure loc

type MyParsec = Parsec ByteString ()




extChars :: [Char]
extChars = "!$%&*+-./:<=>?@^_~"

desugar :: ExprS -> ExprC
desugar s =
  case s of
    VoidS -> VoidC
    NumS i -> NumC i
    VarS s -> VarC s
    lhs `Plus` rhs -> desugar lhs :+: desugar rhs
    lhs `Mult` rhs -> desugar lhs :*: desugar rhs
    lhs `Minus` rhs -> desugar lhs :+: (NumC (-1) :*: desugar rhs)
    Neg i -> NumC (-1) :*: desugar i
    FdefS arg body -> Prelude.foldr  FdefC (desugar body) arg
    AppS fun args -> Prelude.foldl AppC (desugar fun) (fmap desugar args)
    LetS letXs bodyExpr ->
      let f (LetExpr id body) exp = AppC (FdefC id exp) (desugar body)
          expC = desugar bodyExpr
      in Prelude.foldr f expC letXs
    BeginS beginXs -> Prelude.foldr1 SeqC $ fmap desugar beginXs
    SetS expr1 expr2 -> SetC expr1 (desugar expr2)
    IfS cond expr1 expr2 -> IfC (desugar cond) (desugar expr1) (desugar expr2)
    LessS expr1 expr2 thenExpr elseExpr ->
      LessC
        (desugar expr1)
        (desugar expr2)
        (desugar thenExpr)
        (desugar elseExpr)
    LetRecS letXs bodyExpr ->
      let f (LetExpr id body) exp = RecAppC (FdefC id exp) (desugar body)
          expC = desugar bodyExpr
      in Prelude.foldr f expC letXs
    BoolS b -> BoolC b


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
  , P.reservedNames = ["fun", "+", "-", "*", "let", "begin", "set!","blt","letrec","true","false","nil","if","def"]
  , P.reservedOpNames = []
  , P.caseSensitive = True
  }
  

lexer :: P.GenTokenParser ByteString () Identity
lexer = P.makeTokenParser seiDef

defp :: ParsecT ByteString () Identity ()
defp = P.reserved lexer "def"

truep :: ParsecT ByteString () Identity ()
truep = P.reserved lexer "true"

ifp :: ParsecT ByteString () Identity ()
ifp = P.reserved lexer "if"

falsep :: ParsecT ByteString () Identity ()
falsep = P.reserved lexer "false"

letrecp :: ParsecT ByteString () Identity ()
letrecp = P.reserved lexer "letrec"

bltp :: ParsecT ByteString () Identity ()
bltp = P.reserved lexer "blt"

setp :: ParsecT ByteString () Identity ()
setp = P.reserved lexer "set!"

beginp :: ParsecT ByteString () Identity ()
beginp = P.reserved lexer "begin"

letp :: ParsecT ByteString () Identity ()
letp = P.reserved lexer "let"

parens :: ParsecT ByteString () Identity a -> ParsecT ByteString () Identity a
parens = P.parens lexer

identifier :: ParsecT ByteString () Identity String
identifier = P.identifier lexer

fun :: ParsecT ByteString () Identity ()
fun = P.reserved lexer "fun"

integer :: ParsecT ByteString () Identity Integer
integer = P.integer lexer

plus :: ParsecT ByteString () Identity ()
plus = P.reserved lexer "+"

mult :: ParsecT ByteString () Identity ()
mult = P.reserved lexer "*"

minus :: ParsecT ByteString () Identity ()
minus = P.reserved lexer "-"

lexeme :: ParsecT ByteString () Identity a -> ParsecT ByteString () Identity a
lexeme = P.lexeme lexer

brackets :: ParsecT ByteString () Identity a -> ParsecT ByteString () Identity a
brackets = P.brackets lexer

whiteSpace :: ParsecT ByteString () Identity ()
whiteSpace = P.whiteSpace lexer

sexp :: MyParsec ExprS
sexp =
  ((<|>) <$> parens <*> brackets)
    (plusExpr <|> fdefExpr <|> minusExpr <|> multExpr <|> appExpr <|> letExprs <|>
     beginExprs <|>
     setExpr <|>
     bltExpr <|>
     letRecExprs <|>
     ifExpr) <|>
  fmap NumS integer <|>
  try (fmap (VarS . pack) identifier) <|>
  boolExpr
  where
    multExpr = mult *> pure Mult <*> sexp <*> sexp <?> "multiplication"
    plusExpr = plus *> pure Plus <*> sexp <*> sexp <?> "plus"
    fdefExpr = fun *> pure FdefS <*> parens (many1 (fmap pack identifier)) <*> sexp <?> "function definition"
    ifExpr = ifp *> pure IfS <*> sexp <*> sexp <*> sexp
    minusExpr = do
      minus
      lhs <- sexp
      (Minus lhs <$> sexp <?> "Minus") <|> (pure (Neg lhs )<?> "Negation")
    appExpr = AppS <$> sexp <*> many1 sexp <?> "function application"
    letExpr =
      ((<|>) <$> parens <*> brackets)
        (LetExpr <$> fmap pack identifier <*> sexp) <?> "let pair"
    letExprs =
      LetS <$> (letp *> ((<|>) <$> parens <*> brackets) (many1 letExpr)) <*>
      sexp <?> "let expression"
    beginExprs = beginp *> pure BeginS <*> many1 sexp <?> "begin expression"
    setExpr = setp *> pure SetS <*> fmap pack identifier <*> sexp <?> "set expression"
    bltExpr = bltp *> pure LessS <*> sexp <*> sexp <*> sexp <*> sexp <?> "blt expression"
    letRecExprs =
      LetRecS <$> (letrecp *> ((<|>) <$> parens <*> brackets) (many1 letExpr)) <*>
      sexp <?> "letrec expression"
    boolExpr = (truep *> pure (BoolS True) <|> falsep *> pure (BoolS False))
    -- defExpr = defp *> DefS <*> fmap pack identifier <*> sexp <?> "def expression"

    


sexps :: ParsecT ByteString () Identity [ExprS]
sexps = Control.Applicative.many sexp


interpM :: ExprC -> InterpMonad Value
interpM exp =
  case exp of
    NumC i -> pure . NumV $ i
    i :+: j -> fmap NumV $ (+) <$> (extractNumV =<< interpM i) <*> (extractNumV =<< interpM j)
    i :*: j -> fmap NumV $ (*) <$> (extractNumV =<< interpM i) <*> (extractNumV =<< interpM j)
    AppC f arg -> do
      FdefV argId body fenv <- extractFdefV =<< interpM f
      argV <- interpM arg
      loc <- storeV argV
      bodySto <- gets (view sto)
      bodyIds <- gets (view ids)
      let bodyEnv = M.insert argId loc fenv
          bodyState = InterpState{_env = bodyEnv, _sto = bodySto, _ids = bodyIds}
          evalResult = flip runInterp bodyState $ (interpM body)
      case evalResult of
        Left err -> throwError err
        Right (v,InterpState{_sto = sto', _ids = ids'}) -> do
          modify (set ids ids')
          modify (set sto sto')
          pure v
    VarC id ->  fetchV id
    FdefC arg body -> do
      fenv <- gets (M.delete arg . (view env))
      pure $ FdefV arg body fenv
    SeqC exp1 exp2 -> interpM exp1 *> interpM exp2
    SetC var exp -> do
      loc <- fetchLoc var
      interpM exp >>= modifyV loc
    LessC expr1 expr2 thenExpr elseExpr -> do
      v1 <- interpM expr1 >>= extractNumV
      v2 <- interpM expr2 >>= extractNumV
      case v1 < v2 of
        True -> interpM thenExpr
        False -> interpM elseExpr
    IfC cond thenExpr elseExpr -> do
      b <- interpM cond >>= extractBoolV
      case b of
        True -> interpM thenExpr
        False -> interpM elseExpr
    BoolC b -> pure (BoolV b)
    VoidC -> pure VoidV
    
    -- Makes the arg of expr visible to argExpr
    RecAppC expr argExpr -> mdo
      FdefV argId body fenv <- extractFdefV =<< interpM expr
      argLoc <- newLoc
      bodySto <- gets (view sto)
      bodyIds <- gets (view ids)
      extendedEnv <- gets (M.insert argId argLoc .(view env))
      let argState = InterpState{_env = extendedEnv, _sto = IM.insert argLoc argV bodySto, _ids = bodyIds}
          Right (argV,InterpState{_sto=sto',_ids=ids'}) = flip runInterp argState $ (interpM argExpr)
      modify (set ids ids')
      modify (set sto sto')
      let bodyEnv = M.insert argId argLoc fenv
      let bodyState = InterpState{_env = bodyEnv, _sto = sto', _ids = bodyIds}
      let evalResult = flip runInterp bodyState $ (interpM body)
      case evalResult of
        Left err -> throwError err
        Right (v,InterpState{_sto = sto'', _ids = ids''}) -> do
          modify (set ids ids'')
          modify (set sto sto'')
          pure v
      
interp' :: ExprC -> Either String Value
interp' expr =
  let result = runInterp (interpM expr) emptyState
  in case result of
    Left err -> Left err
    Right (v,_) -> Right v



runParserLexeme :: MyParsec a -> ByteString -> Either ParseError a
runParserLexeme p = runParser (whiteSpace *> p) () ""
