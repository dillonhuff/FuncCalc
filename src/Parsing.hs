module Parsing(
	parseProgram,
	parseExpr,
	FuncDef(FD),
	Expr(Num, FunCall),
	FCTok(FuncTok, NumTok, RParen, LParen, FuncStart, Assign)) where

import Text.ParserCombinators.Parsec

type FCProgram = [FuncDef]
type FuncName = String
type FuncArg = String

data FuncDef = FD FuncName [FuncArg] Expr
	deriving (Eq, Show)

data Expr
	= Num Int
	| FunCall FuncName [Expr]
	deriving (Eq, Show)

parseProgram :: String -> FCProgram
parseProgram str = fst $ head $ pProgram $ programToks str

pProgram :: FCParser FCProgram
pProgram = pZeroOrMore pFuncDef

pFuncDef :: FCParser FuncDef
pFuncDef = pThen makeFuncDef pFuncNameAndArgs (pThen ignoreFst (pTok Assign) pExpr)

pTok :: FCTok -> FCParser FCTok
pTok s (t:toks) = if s == t
	then [(t, toks)]
	else []
pTok s [] = []

makeFuncDef :: [String] -> Expr -> FuncDef
makeFuncDef (name:args) expr = FD name args expr
makeFuncDef _ _ = error "function definition not correcttly formed"
		
pFuncNameAndArgs :: FCParser [String]
pFuncNameAndArgs = pThen ignoreFst (pTok FuncStart) (pOneOrMore pLit)

appendFst :: a -> [a] -> [a]
appendFst a aList = a:aList

ignoreFst :: a -> b -> b
ignoreFst a b = b

parseExpr :: String -> Expr
parseExpr str = fst $ head $ pExpr $ programToks str

pExpr :: FCParser Expr
pExpr [] = []
pExpr ((NumTok n):rest) = [(Num n, rest)]
pExpr (LParen:rest) = [(parenExpr, afterParenExpr)]
	where
		parenExpr = fst $ head $ pExpr $ beforeParen 1 rest
		afterParenExpr = afterParen 1 rest
pExpr toks = pFuncall toks

pFuncall :: FCParser Expr
pFuncall = pThen makeFunCall pFID (pZeroOrMore  pArg)

pArg :: FCParser Expr
pArg [] = []
pArg ((NumTok n):rest) = [(Num n, rest)]
pArg ((FuncTok name):rest) = [(FunCall name [], rest)]
pArg (LParen:rest) = [(parenExpr, afterParenExpr)]
	where
		parenExpr = fst $ head $ pExpr $ beforeParen 1 rest
		afterParenExpr = afterParen 1 rest
pArg other = error ("out of options for " ++ show other)

afterParen :: Int -> [FCTok] -> [FCTok]
afterParen n (RParen:ts) = if n == 1
	then ts
	else afterParen (n-1) ts
afterParen n (LParen:ts) = afterParen (n+1) ts
afterParen n (t:ts) = afterParen n ts

beforeParen :: Int -> [FCTok] -> [FCTok]
beforeParen n (RParen:ts) = if n == 1
	then []
	else RParen:(beforeParen (n-1) ts)
beforeParen n (LParen:ts) = LParen:(beforeParen (n+1) ts)
beforeParen n (t:ts) = t:(beforeParen n ts)

makeFunCall :: FuncName -> [Expr] -> Expr
makeFunCall name args = FunCall name args

pFID :: FCParser String
pFID ((FuncTok name):rest) = [(name, rest)]
pFID (t:ts) = error ("function call cannot begin with " ++ show t)

type FCParser a = [FCTok] -> [(a, [FCTok])]

pLit :: FCParser String
pLit [] = []
pLit (tok:toks) = case tok of
	(FuncTok name) -> [(name, toks)]
	_ -> []

pVar :: FCParser FCTok
pVar [] = []
	
pAlt :: FCParser a -> FCParser a -> FCParser a
pAlt p1 p2 = (\toks -> (p1 toks) ++ (p2 toks))

pThen :: (a -> b -> c) -> FCParser a -> FCParser b -> FCParser c
pThen combine p1 p2 =
	(\toks -> [(combine v1 v2, toks2) | (v1,toks1) <- p1 toks, (v2,toks2) <- p2 toks1])

pZeroOrMore :: FCParser a -> FCParser [a]
pZeroOrMore p = (pOneOrMore p) `pAlt` (pEmpty [])

pOneOrMore :: FCParser a -> FCParser [a]
pOneOrMore p = pThen combP p (pZeroOrMore p)

combP :: a -> [a] -> [a]
combP aVal aList = aVal:aList

pEmpty :: a -> FCParser a
pEmpty t = (\input -> [(t, input)])

pApply :: FCParser a -> (a -> b) -> FCParser b
pApply p f = map (applyF f) . p
	where
		applyF f (v, t) = (f v, t)

data FCTok
	= FuncTok String
	| NumTok Int
	| RParen
	| LParen
	| FuncStart
	| Assign
	deriving (Eq, Show)
	
programToks :: String -> [FCTok]
programToks str = case parse fcTokens "Func Calc" str of
	Left err -> error (show err)
	Right toks -> toks
	
fcTokens :: Parser [FCTok]
fcTokens = do
	spaces
	toks <- sepBy fcToken spaces
	spaces
	return toks
	
fcToken :: Parser FCTok
fcToken = do
	tok <-  integer
		<|> funcId
		<|> specialChar
	return tok

integer :: Parser FCTok
integer = do
	numStr <- many1 digit
	return $ NumTok $ read numStr
	
funcId :: Parser FCTok
funcId = do
	start <- letter
	rest <- many alphaNum
	return $ FuncTok (start:rest)
	
specialChar :: Parser FCTok
specialChar = do
	spec <- try (string "==")
	  <|> try (string "=")
	  <|> try (string "<=")
	  <|> try (string "<-")
	  <|> try (string "<")
	  <|> try (string ">=")
	  <|> try (string ">")
	  <|> try (string "/=")
	  <|> string "("
	  <|> string ")"
	  <|> string "#"
	  <|> string "/"
	  <|> string "+"
	  <|> string "-"
	  <|> string "*"
	return $ case spec of
		")" -> RParen
		"(" -> LParen
		"#" -> FuncStart
		"<-" -> Assign
		_ -> FuncTok spec
	
lp :: Parser FCTok
lp = do
	x <- char '('
	return LParen

rp :: Parser FCTok
rp = do
	x <- char ')'
	return RParen
	
funcStart :: Parser FCTok
funcStart = do
	x <- char '#'
	return FuncStart