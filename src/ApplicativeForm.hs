module ApplicativeForm(
	AExpr(AAp, ANum, AFun),
	toApForm) where

import Parsing

data AExpr
	= AAp AExpr AExpr
	| ANum Int
	| AFun String
	deriving (Eq, Show)

toApForm :: Expr -> AExpr
toApForm (Num val) = ANum val
toApForm (FunCall name []) = AFun name
toApForm (FunCall name args) = foldl aapTree (AFun name) (map toApForm args)
	where
		aapTree apTree nextTree = AAp apTree nextTree