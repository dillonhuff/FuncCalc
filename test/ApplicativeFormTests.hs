module ApplicativeFormTests() where

import ApplicativeForm
import Parsing
import Test.HUnit

applicativeFormTests = runTestTT tests

tests = TestList [
	toApForm_num,
	toApForm_num2,
	toApForm_oneArgFunc,
	toApForm_oneNestedArgFunc,
	toApForm_twoArgFunc]

numExpr = Num 980
toApForm_num = TestCase
	(assertEqual "number 980" (ANum 980) (toApForm numExpr))
	
numExpr2 = Num 111
toApForm_num2 = TestCase
	(assertEqual "number 111" (ANum 111) (toApForm numExpr2))
	
toApForm_oneArgFunc = TestCase
	(assertEqual "one argument function"
		(AAp (AFun "f") (ANum 12))
		(toApForm (FunCall "f" [(Num 12)])))
		
toApForm_oneNestedArgFunc = TestCase
	(assertEqual "one nested argument"
		(AAp (AFun "f") (AAp (AFun "g") (ANum 3)))
		(toApForm (FunCall "f" [(FunCall "g" [(Num 3)])])))
		
toApForm_twoArgFunc = TestCase
	(assertEqual "two args"
		(AAp (AAp (AFun "hello") (ANum 4)) (ANum 7))
		(toApForm (FunCall "hello" [(Num 4), (Num 7)])))