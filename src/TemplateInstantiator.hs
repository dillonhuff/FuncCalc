module TemplateInstantiator() where

import Data.List
import ApplicativeForm
import Utils

compile :: Program -> TiState
compile program = (initialStack, initHeap, globals)
	where
		scDefs = program
		(initHeap, globals) = initialHeap scDefs
		initialStack = [addressOfMain]
		addressOfMain = case lookup "main" globals of
			Just mAddr -> mAddr
			Nothing -> error "No address for main"