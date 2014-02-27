module ParsingTests() where

import Test.HUnit
import Parsing

parsingTests = runTestTT tests

tests = TestList []