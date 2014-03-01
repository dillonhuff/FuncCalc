module ParsingTests() where

import Test.HUnit
import Parsing

parsingTests = runTestTT tests

tests = TestList [
	parseExpr_num,
	parseExpr_funCallNoParen,
	parseExpr_funCallNumsAndFuncs,
	parseExpr_funCallParens,
	parseExpr_funCallNestedParens,
	parseProgram_funcNum,
	parseProgram_nestedFunc]

parseExpr_num = TestCase
	(assertEqual "number" (Num 12) (parseExpr "12"))
	
parseExpr_funCallNoParen = TestCase
	(assertEqual "function call without parens"
		(FunCall "f" [(Num 12), (Num 4), (Num 2)])
		(parseExpr "f 12 4 2"))
		
parseExpr_funCallNumsAndFuncs = TestCase
	(assertEqual "function call with mixed args"
		(FunCall "nope" [(FunCall "a" []), (Num 1), (FunCall "bcde" [])])
		(parseExpr "nope a 1 bcde"))
		
parseExpr_funCallParens = TestCase
	(assertEqual "function call with parens"
		(FunCall "kewl" [(FunCall "n" [(Num 1), (Num 2)]), (Num 2)])
		(parseExpr "kewl (n 1 2) 2"))
		
parseExpr_funCallNestedParens = TestCase
	(assertEqual "function call with nested parens"
		(FunCall "k" [(FunCall "n" [(Num 1), (FunCall "j" [(Num 2), (FunCall "no" [])])]), (Num 1), (Num 2)])
		(parseExpr "k (n 1 (j 2 no)) 1 2"))
		
parseProgram_funcNum = TestCase
	(assertEqual "func def as number"
		[(FD "f" [] (Num 12))]
		(parseProgram "# f <- 12"))
		
parseProgram_nestedFunc = TestCase
	(assertEqual "func def with nested expr"
		[FD "g" ["a", "how"]
			(FunCall "oooh"
				[(FunCall "no" []), (Num 45), (FunCall "how" [(FunCall "a" [])])])]
		(parseProgram "# g a how <- oooh no 45 (how a)"))