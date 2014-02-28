module Parsing(
	pProgram,
	pExpr,
	Expr(Num, FunCall, FunDef),
	FCTok(FuncTok, NumTok, RParen, LParen, FuncStart, Assign)) where

import Text.ParserCombinators.Parsec

type FCProgram = [Expr]
type FuncName = String
type FuncArg = String

data Expr
	= Num Int
	| FunCall FuncName [Expr]
	| FunDef FuncName [FuncArg] Expr
	deriving (Eq, Show)

pProgram :: String -> FCProgram
pProgram str = [fst $ head $ pExpr $ programToks str]

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

pLit :: FCTok -> FCParser FCTok
pLit s [] = []
pLit s (tok:toks) = if s == tok
	then [(s, toks)]
	else []

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