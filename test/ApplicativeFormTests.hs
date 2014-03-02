module ApplicativeFormTests() where

import Parsing
import Test.HUnit

applicativeFormTests = runTestTT tests

tests = TestList []

toApForm_num = TestCase
	(assertEqual "number" (Num 980) (toApForm