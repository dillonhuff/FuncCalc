module ParsingTests() where

import Test.HUnit
import Parsing

parsingTests = runTestTT tests

tests = TestList [
	pExpr_num,
	pExpr_funCallNoParen,
	pExpr_funCallNumsAndFuncs,
	pExpr_funCallParens,
	pExpr_funCallNestedParens]

pExpr_num = TestCase
	(assertEqual "number" [(Num 12)] (pProgram "12"))
	
pExpr_funCallNoParen = TestCase
	(assertEqual "function call without parens"
		[(FunCall "f" [(Num 12), (Num 4), (Num 2)])]
		(pProgram "f 12 4 2"))
		
pExpr_funCallNumsAndFuncs = TestCase
	(assertEqual "function call with mixed args"
		[(FunCall "nope" [(FunCall "a" []), (Num 1), (FunCall "bcde" [])])]
		(pProgram "nope a 1 bcde"))
		
pExpr_funCallParens = TestCase
	(assertEqual "function call with parens"
		[(FunCall "kewl" [(FunCall "n" [(Num 1), (Num 2)]), (Num 2)])]
		(pProgram "kewl (n 1 2) 2"))
		
pExpr_funCallNestedParens = TestCase
	(assertEqual "function call with nested parens"
		[FunCall "k" [(FunCall "n" [(Num 1), (FunCall "j" [(Num 2), (FunCall "no" [])])]), (Num 1), (Num 2)]]
		(pProgram "k (n 1 (j 2 no)) 1 2"))