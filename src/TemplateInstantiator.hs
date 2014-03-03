module TemplateInstantiator() where

import Data.List
import ApplicativeForm

compile :: Program -> TiState
compile program = (initialStack, initHeap, globals)
	where
		scDefs = program
		(initHeap, globals) = initialHeap scDefs
		initialStack = [addressOfMain]
		addressOfMain = case lookup "main" globals of
			Just mAddr -> mAddr
			Nothing -> error "No address for main"

type TiState = (TiStack, TiHeap, TiGlobals)

type TiStack = [Addr]
type TiHeap = Heap Node
type TiGlobals = [(Name, Addr)]

type Addr = Int

showAddr :: Addr -> String
showAddr addr = "#" ++ show addr

type Name = String

data Node
	= NAp Addr Addr
	| NSup Name [Name] AExpr
	| NNum Int

type Heap a = (Int, [Int], [(Int, a)])

initialHeap :: [SCDef] -> (TiHeap, TiGlobals)
initialHeap scDefs = mapAccumL allocateSc hInitial scDefs

allocateSc :: TiHeap -> SCDef -> (TiHeap, (Name, Addr))
allocateSc heap (SC name args body) = 
	(newHeap, (name, addr))
	where
		(newHeap, addr) = hAlloc heap (NSup name args body)

hInitial :: Heap a
hInitial = (0, [1..], [])

hAlloc :: Heap a -> a -> (Heap a, Addr)
hAlloc (size, (next:free), cts) n = ((size+1, free, (next,n) : cts),next)

hUpdate :: Heap a -> Addr -> a -> Heap a
hUpdate (size, free, cts) a n = (size, free, (a,n) : remove cts a)

hFree :: Heap a -> Addr -> Heap a
hFree (size, free, cts) a = (size-1, a:free, remove cts a)

hLookup :: Heap a -> Addr -> a
hLookup (size,free,cts) a = case lookup a cts of
	Just val -> val
	Nothing -> error ("can’t find node " ++ show a ++ " in heap")

hAddresses :: Heap a -> [Addr]
hAddresses (size, free, cts) = [addr | (addr, node) <- cts]

hSize :: Heap a -> Int
hSize (size, free, cts) = size

remove :: [(Int,a)] -> Int -> [(Int,a)]
remove [] a = error ("Attempt to update or free nonexistent address #" ++ showAddr a)
remove ((ap,n):cts) a | a == ap = cts
					  | otherwise = (ap,n) : remove cts a