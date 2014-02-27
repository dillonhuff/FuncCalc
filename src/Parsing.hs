module Parsing() where

import Text.ParserCombinators.Parsec

type FCProgram = [FCFunc]

type FuncName = String
type FuncArg = String
type Expr = Int | 
type FCFunc = (FuncName, Expr)

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
		"(" -> RParen
		")" -> LParen
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