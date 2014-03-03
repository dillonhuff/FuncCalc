module ApplicativeForm(
	makeProgram,
	Program, SCDef(SC),
	AExpr(AAp, ANum, AFun),
	toApForm) where

import Parsing

type Program = [SCDef]
data SCDef = SC String [String] AExpr
	deriving (Eq, Show)

data AExpr
	= AAp AExpr AExpr
	| ANum Int
	| AFun String
	deriving (Eq, Show)

makeProgram :: FCProgram -> Program
makeProgram fcProg = map applicativeForm fcProg

applicativeForm :: FuncDef -> SCDef
applicativeForm (FD name args expr) = SC name args $ toApForm expr

toApForm :: Expr -> AExpr
toApForm (Num val) = ANum val
toApForm (FunCall name []) = AFun name
toApForm (FunCall name args) = foldl aapTree (AFun name) (map toApForm args)
	where
		aapTree apTree nextTree = AAp apTree nextTree