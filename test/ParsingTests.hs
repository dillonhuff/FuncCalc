module ParsingTests() where

import Test.HUnit
import Parsing

parsingTests = runTestTT tests

tests = TestList [
	pExpr_num,
	pExpr_funcallNoParen]

pExpr_num = TestCase
	(assertEqual "number" [(Num 12)] (pProgram "12"))
	
pExpr_funcallNoParen = TestCase
	(assertEqual "function call without parens"
		[(FunCall "f" [(Num 12), (Num 4), (Num 2)])]
		(pProgram "f 12 4 2"))