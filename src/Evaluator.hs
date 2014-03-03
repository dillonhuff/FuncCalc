module Evaluator() where

import ApplicativeForm
import Utils

eval state = state : restStates
	where
		restStates | tiFinal state = []
				   | otherwise = eval nextState
		nextState = step state
		
tiFinal :: TiState -> Bool
tiFinal ([soleAddr], heap, globals) = isDataNode (hLookup heap soleAddr)
tiFinal ([], _, _) = error "Empty stack!"
tiFinal state = False

isDataNode :: Node -> Bool
isDataNode (NNum _) = True
isDataNode _ = False

step :: TiState -> TiState
step state = dispatch (hLookup heap (head stack))
	where
		(stack, heap, globals) = state
		dispatch (NNum n) = numStep state n
		dispatch (NAp a1 a2) = apStep state a1 a2
		dispatch (NSup sc args body) = scStep state sc args body
		
-- A number appearing on the top of the stack is an error
numStep :: TiState -> Int -> TiState
numStep _ n = error ("A number, " ++ show n ++ ", is on the top of the stack")

apStep :: TiState -> Addr -> Addr -> TiState
apStep (stack, heap, globals) a1 a2 = (a1:stack, heap, globals)

scStep :: TiState -> String -> [String] -> AExpr -> TiState
scStep (stack, heap, globals) name args body
	= (newStack, newHeap, globals)
	where
		newStack = resultAddr:(drop (length args+1) stack)
		(newHeap, resultAddr) = instantiate body heap env
		env = argBindings ++ globals
		argBindings = zip args (getargs heap stack)
		
getargs :: TiHeap -> TiStack -> [Addr]
getargs heap (sc:stack) = map (getarg heap) stack

getarg heap addr = arg
	where
		(NAp func arg) = hLookup heap addr
		
instantiate :: AExpr -> TiHeap -> [(Name, Addr)] -> (TiHeap, Addr)
instantiate (ANum n) heap env = hAlloc heap (NNum n)