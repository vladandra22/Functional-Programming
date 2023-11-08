module TestBinomialHeap where

import Data.Maybe (Maybe)

import BinomialHeap
import TestPP

{-
    Construiește un arbore binomial de rang dat, începând cu prioritatea și cu cheia
    date.
-}
dummyTreeOfRank :: (Num p, Num k) => Int -> p -> k -> BinomialTree p k
dummyTreeOfRank 0 p k = Node p k []
dummyTreeOfRank rank startPrio startKey = top{children=bot : children top}
    where
        top = dummyTreeOfRank (rank-1) startPrio startKey
        bot = dummyTreeOfRank (rank-1) (startPrio+1) (startKey + 2^(rank-1))

{-
    Construiește o listă de arbori binomiali de ranguri date.
    Sunt permise rangurile negative, iar acestea dau naștere unor arbori vizi.
-}
dummyTrees :: (Num p, Num k) => [Int] -> [BinomialTree p k]
dummyTrees = map buildTree
    where
        buildTree rank
            | rank < 0 = EmptyTree
            | otherwise = dummyTreeOfRank rank 0 0

{-
    Construiește un heap binomial, folosindu-se de funcția anterioară.
-}
dummyHeap :: (Num p, Num k) => [Int] -> BinomialHeap p k
dummyHeap lst = BinomialHeap size trees
    where
        trees = dummyTrees lst
        size = sum (map getTreeSize trees)

{-
    Verifică proprietatea de arbore binomial care respectă suplimentar
    proprietatea de heap.
-}
treeProperty :: Ord p => BinomialTree p k -> Bool
treeProperty EmptyTree = True
treeProperty tree =
    let
        noChildren = length $ children tree
        condNoChildren = f (children tree) (noChildren - 1)
            where
                f [] (-1) = True
                f (tree : trees) n = (length (children tree) == n) && f trees (n - 1)
                f _ _ = False
        condHeap = null (children tree) || prio tree <= minimum (map prio (children tree))
    in
        condHeap && foldl (&&) condNoChildren (map treeProperty (children tree))

{-
    Verifică proprietatea de heap binomial: arborii sunt testați cu funcția
    de mai sus, iar dimensiunea heap-ului trebuie să coincidă cu suma
    dimensiunilor arborilor.
-}
binomialHeapProperty :: (Ord p, Eq k) => BinomialHeap p k -> Bool
binomialHeapProperty heap = size heap == x && size heap == y && all treeProperty (trees heap)
    where
        x = sum $ map ((2^) . length . children) $ filter (/= EmptyTree) $ trees heap
        y = sum $ map getTreeSize $ filter (/= EmptyTree) $ trees heap

{-
    Calculează dimensiunea unui arbore binomial, fără a presupune corectitudinea
    construcției acestuia.
-}
getTreeSize :: BinomialTree p k -> Int
getTreeSize EmptyTree = 0
getTreeSize (Node prio key children) = 1 + sum (map getTreeSize children)

{-
    Verifică dacă o listă de arbori binomiali respectă proprietățile de heap binomial.
-}
checkResult :: (Ord p, Eq k) => [BinomialTree p k] -> Bool
checkResult trees = binomialHeapProperty (BinomialHeap (sum (map getTreeSize trees)) trees)

testAttach :: TestData
testAttach = tests 1 15
    [
        testVal "attach EmptyTree" (EmptyTree :: BinomialTree Int Char) $ attach EmptyTree EmptyTree,

        testVal "attach example 1" nodeA{children=[nodeB]} $ attach nodeA nodeB,
        testVal "attach example 2" nodeA{children=[nodeB]} $ attach nodeB nodeA,

        testVal "attach rank 1" nodeCDEF $ attach nodeCD nodeEF,
        testVal "attach rank 2" nodeGHIJKLMN $ attach nodeGHIJ nodeKLMN,

        testCond "attach larger" $ treeProperty largerAttached && getTreeSize largerAttached == 2^7
    ]
        where
            nodeA = Node 0 'a' []
            nodeB = Node 1 'b' []

            nodeCD = Node 0 'c' [Node 1 'd' []]
            nodeEF = Node 1 'e' [Node 3 'f' []]
            nodeCDEF = nodeCD{children = nodeEF : children nodeCD}

            nodeGHIJ = Node 5 'g' [Node 5 'h' [Node 7 'j' []], Node 6 'i' []]
            nodeKLMN = Node 2 'k' [Node 3 'l' [Node 3 'n' []], Node 4 'm' []]
            nodeGHIJKLMN = nodeKLMN{children = nodeGHIJ : children nodeKLMN}

            nodeLarger1 = dummyTreeOfRank 6 0 0
            nodeLarger2 = dummyTreeOfRank 6 0 64
            largerAttached = attach nodeLarger1 nodeLarger2

testInsertTree :: TestData
testInsertTree = tests 2 25
    [
        testVal "insertTree example 1" [nodeA]                              treeA,
        testVal "insertTree example 2" [EmptyTree, nodeA{children=[nodeB]}] treeAB,
        testVal "insertTree example 3" [nodeC, nodeA{children=[nodeB]}]     treeABC,

        testCond "insertTree domino 11->001"    $ obeysMask 0 [0,0,1] $ insertTree (Node 0 0 []) $ dummyTrees [0,1],
        testCond "insertTree domino 101->011"   $ obeysMask 0 [0,1,1] $ insertTree (Node 0 0 []) $ dummyTrees [0,-1,2],
        testCond "insertTree domino 110->001"   $ obeysMask 0 [0,0,1] $ insertTree (Node 0 0 []) $ dummyTrees [0,1,-1],
        testCond "insertTree domino 1101->0011" $ obeysMask 0 [0,0,1,1] $ insertTree (Node 0 0 []) $ dummyTrees [0,1,-1,3],
        testCond "insertTree domino 111->0001"  $ obeysMask 4 [0,0,0,1] $ insertTree (dummyTreeOfRank 4 0 0) $ dummyTrees [4..6],

        testCond "insertTree large simple" $ obeysMask 4 (replicate 8 1) $ insertTree (dummyTreeOfRank 4 0 0) $ EmptyTree : dummyTrees [5..11],
        testCond "insertTree large domino" $ obeysMask 4 (replicate 7 0 ++ [1]) $ insertTree (dummyTreeOfRank 4 0 0) $ dummyTrees [4..10]
    ]
        where
            nodeA = Node 1 'a' []
            nodeB = Node 2 'b' []
            nodeC = Node 3 'c' []
            treeA = insertTree nodeA []
            treeAB = insertTree nodeB treeA
            treeABC = insertTree nodeC treeAB

            treesLarge = dummyTrees [4..11]

            -- |Checks if a list of binomial trees respects a binary mask where
            -- 0 represents a missing tree, and 1 represents a present tree.
            -- The trees should be valid and have the right rank.
            obeysMask :: (Ord p) => Int -> [Int] -> [BinomialTree p k] -> Bool
            obeysMask startRank mask trees = okLength && okPositions
                where
                    okLength = length mask == length trees
                    okPositions = and $ zipWith validPos [startRank..] $ zip mask trees
                    validPos _ (0, EmptyTree) = True
                    validPos rank (1, node@Node{}) = treeProperty node && treeSize node == 2^rank
                    validPos _ _ = False

            treeSize :: BinomialTree p k -> Int
            treeSize EmptyTree = 0
            treeSize Node{children=children} = 2 ^ length children

testEmptyHeap :: TestData
testEmptyHeap = tests 3 5
    [
        testCond "test emptyHeap" ((size emptyHeap == 0) && null (trees emptyHeap))
    ]

testInsert :: TestData
testInsert = tests 4 10
    [
        testCond "insert 1 element" (binomialHeapProperty expr1),
        testVal "insert 1 element (exact)" res1 expr1,
        testCond "insert 2 elements" (binomialHeapProperty expr2),
        testVal "insert 2 elements (exact)" res2 expr2,
        testCond "insert 3 elements out of order" (binomialHeapProperty expr3),
        testVal "insert 3 elements out of order (exact)" res3 expr3,
        testCond "insert 4 elements" (binomialHeapProperty expr4),
        testVal "insert 4 elements (exact)" res4 expr4,
        testCond "insert 6 elements" (binomialHeapProperty expr5),
        testVal "insert 6 elements (exact)" res5 expr5
    ]
        where
            expr1 = insert 1 'a' emptyHeap
            expr2 = insert 2 'b' $ insert 1 'a' emptyHeap
            expr3 = insert 2 'c' $ insert 1 'b' $ insert 3 'a' emptyHeap
            expr4 = insert 1 'a' $ insert 2 'b' $ insert 3 'c' $ insert 4 'd' emptyHeap
            expr5 = insert 100 'f' $ insert 70 'e' $ insert 200 'd' $ insert 60 'c' $ insert 80 'b' $ insert 21 'a' emptyHeap

            res1 = BinomialHeap {size = 1, trees = [Node {prio = 1, key = 'a', children = []}]}
            res2 = BinomialHeap {size = 2, trees = [EmptyTree,Node {prio = 1, key = 'a', children = [Node {prio = 2, key = 'b', children = []}]}]}
            res3 = BinomialHeap {size = 3, trees = [Node {prio = 2, key = 'c', children = []},Node {prio = 1, key = 'b', children = [Node {prio = 3, key = 'a', children = []}]}]}
            res4 = BinomialHeap {size = 4, trees = [EmptyTree,EmptyTree,Node {prio = 1, key = 'a', children = [Node {prio = 3, key = 'c', children = [Node {prio = 4, key = 'd', children = []}]},Node {prio = 2, key = 'b', children = []}]}]}
            res5 = BinomialHeap {size = 6, trees = [EmptyTree,Node {prio = 70, key = 'e', children = [Node {prio = 100, key = 'f', children = []}]},Node {prio = 21, key = 'a', children = [Node {prio = 60, key = 'c', children = [Node {prio = 200, key = 'd', children = []}]},Node {prio = 80, key = 'b', children = []}]}]}

testFindMin :: TestData
testFindMin = tests 5 25
    [
        testVal "test findMin EmptyTree" val0 expr0,
        testVal "test findMin 3 elements" val1 expr1,
        testVal "test findMin 4 elements basic" val2 expr2,
        testVal "test findMin 4 elements insert 1 last" val3 expr3,
        testVal "test findMin 4 elements insert 1 penultimate" val4 expr4,
        testVal "test findMin 5 elements basic" val5 expr5,
        testVal "test findMin 5 elements change key order" val6 expr6,
        testVal "test findMin 5 elements change priority insertion order" val7 expr7
    ]
        where
            val0 = Nothing :: Maybe (Int, String)
            val1 = Just (1,'a')
            val2 = Just (1,'a')
            val3 = Just (1,'d')
            val4 = Just (1,'a')
            val5 = Just (1,'a')
            val6 = Just (1,'b')
            val7 = Just (1,'h')
            expr0 = findMin emptyHeap
            expr1 = findMin $ BinomialHeap 3 [Node 3 'c' [], Node 1 'a' [Node 2 'b' []]]
            expr2 = findMin $ BinomialHeap 4 [EmptyTree, EmptyTree, Node 1 'a' [Node 3 'c' [Node 4 'd' []], Node 2 'b' []]]
            expr3 = findMin $ BinomialHeap 4 [EmptyTree, EmptyTree, Node 1 'd' [Node 2 'a' [Node 3 'b' []], Node 4 'c' []]]
            expr4 = findMin $ BinomialHeap 4 [EmptyTree, EmptyTree, Node 1 'a' [Node 2 'b' [Node 3 'c' []], Node 4 'd' []]]
            expr5 = findMin $ BinomialHeap 5 [Node 5 'e' [], EmptyTree, Node 1 'a' [Node 3 'c' [Node 4 'd' []], Node 2 'b' []]]
            expr6 = findMin $ BinomialHeap 5 [Node 5 'h' [], EmptyTree, Node 1 'b' [Node 3 'g' [Node 4 'f' []], Node 2 'a' []]]
            expr7 = findMin $ BinomialHeap 5 [Node 1 'h' [], EmptyTree, Node 2 'f' [Node 4 'a' [Node 5 'b' []], Node 3 'g' []]]

testMergeTrees :: TestData
testMergeTrees = tests 7 30
    [
        testCond "mergeTrees empty lists" $ checkResult expr1,
        testVal "mergeTrees empty lists (exact)" res1 expr1,
        testCond "mergeTrees empty trees" $ checkResult expr2,
        testVal "mergeTrees empty trees (exact)" res2 expr2,
        testCond "mergeTrees rank 0" $ checkResult expr3,
        testVal "mergeTrees rank 0 (exact)" res3 expr3,
        testCond "mergeTrees rank 1 - no carry" $ checkResult expr4,
        testVal "mergeTrees rank 1 - no carry (exact)" res4 expr4,
        testCond "mergeTrees rank 1 - carry" $ checkResult expr5,
        testVal "mergeTrees rank 1 - carry (exact)" res5 expr5,
        testCond "mergeTrees rank 2 - first longer" $ checkResult expr6,
        testVal "mergeTrees rank 2 - first longer (exact)" res6 expr6,
        testCond "mergeTrees rank 2 - second longer" $ checkResult expr7,
        testVal "mergeTrees rank 2 - second longer (exact)" res7 expr7,
        testCond "mergeTrees rank 2 - no carry" $ checkResult expr8,
        testVal "mergeTrees rank 2 - no carry (exact)" res8 expr8,
        testCond "mergeTrees rank 3 - equal priorities" $ checkResult expr9,
        testVal "mergeTrees rank 3 - equal priorities (exact)" res9 expr9,
        testCond "mergeTrees rank 3 - carry" $ checkResult expr10,
        testVal "mergeTrees rank 3 - carry (exact)" res10 expr10
    ]
        where
            expr1 = mergeTrees [] [] :: [BinomialTree Int Int]
            expr2 = mergeTrees [EmptyTree] [EmptyTree] :: [BinomialTree Int Int]
            expr3 = mergeTrees [Node 1 2 []] [Node 2 4 []]
            expr4 = mergeTrees [EmptyTree, Node 1 'a' [Node 5 'b' []]] [Node 9 'c' []]

            expr5 = mergeTrees [EmptyTree, Node 4 7.98 [Node 7.5 12.3 []]] [Node 0.2 3.08 [], Node 21.38 43.8 [Node 74.28 3.9 []]]
            expr6 = mergeTrees [EmptyTree, dummyTreeOfRank 1 3 4, dummyTreeOfRank 2 5 2] [Node 5 7 [], dummyTreeOfRank 1 4 6]
            expr7 = mergeTrees [EmptyTree, Node 3 9 [Node 4 11 []]] [Node 2 5 [], Node 7 20 [Node 8 12 []], dummyTreeOfRank 2 6 4]
            expr8 = mergeTrees [Node 1 5 [], EmptyTree, dummyTreeOfRank 2 56 73] [EmptyTree, Node 11 22 []]
            expr9 = mergeTrees tree1 tree2
                where
                    tree1 = [EmptyTree, Node 1 5 [Node 2 3 []], dummyTreeOfRank 2 8 (-1)]
                    tree2 = [Node 10 (-2) [], Node 1 8 [Node 2 15 []], dummyTreeOfRank 2 9 47, dummyTreeOfRank 3 98 23]
            expr10 = mergeTrees tree1' tree2'
                where
                    tree1' = [Node 4 8 [], Node 7 12 [Node 8 11 []], dummyTreeOfRank 2 5 (-3), dummyTreeOfRank 3 10 6]
                    tree2' = [Node 2 1 [], Node 9 10 [Node 19 29 []], dummyTreeOfRank 2 18 9, dummyTreeOfRank 3 6 100]

            res1 = []
            res2 = [EmptyTree]
            res3 = [EmptyTree,Node {prio = 1, key = 2, children = [Node {prio = 2, key = 4, children = []}]}]
            res4 = [Node {prio = 9, key = 'c', children = []},Node {prio = 1, key = 'a', children = [Node {prio = 5, key = 'b', children = []}]}]
            res5 = [Node {prio = 0.2, key = 3.08, children = []},EmptyTree,Node {prio = 4.0, key = 7.98, children = [Node {prio = 21.38, key = 43.8, children = [Node {prio = 74.28, key = 3.9, children = []}]},Node {prio = 7.5, key = 12.3, children = []}]}]
            res6 = [Node {prio = 5, key = 7, children = []},EmptyTree,EmptyTree,Node {prio = 3, key = 4, children = [Node {prio = 5, key = 2, children = [Node {prio = 6, key = 4, children = [Node {prio = 7, key = 5, children = []}]},Node {prio = 6, key = 3, children = []}]},Node {prio = 4, key = 6, children = [Node {prio = 5, key = 7, children = []}]},Node {prio = 4, key = 5, children = []}]}]
            res7 = [Node {prio = 2, key = 5, children = []},EmptyTree,EmptyTree,Node {prio = 3, key = 9, children = [Node {prio = 6, key = 4, children = [Node {prio = 7, key = 6, children = [Node {prio = 8, key = 7, children = []}]},Node {prio = 7, key = 5, children = []}]},Node {prio = 7, key = 20, children = [Node {prio = 8, key = 12, children = []}]},Node {prio = 4, key = 11, children = []}]}]
            res8 = [Node {prio = 1, key = 5, children = []},Node {prio = 11, key = 22, children = []},Node {prio = 56, key = 73, children = [Node {prio = 57, key = 75, children = [Node {prio = 58, key = 76, children = []}]},Node {prio = 57, key = 74, children = []}]}]
            res9 = [Node {prio = 10, key = -2, children = []},EmptyTree,Node {prio = 1, key = 5, children = [Node {prio = 1, key = 8, children = [Node {prio = 2, key = 15, children = []}]},Node {prio = 2, key = 3, children = []}]},EmptyTree,Node {prio = 8, key = -1, children = [Node {prio = 98, key = 23, children = [Node {prio = 99, key = 27, children = [Node {prio = 100, key = 29, children = [Node {prio = 101, key = 30, children = []}]},Node {prio = 100, key = 28, children = []}]},Node {prio = 99, key = 25, children = [Node {prio = 100, key = 26, children = []}]},Node {prio = 99, key = 24, children = []}]},Node {prio = 9, key = 47, children = [Node {prio = 10, key = 49, children = [Node {prio = 11, key = 50, children = []}]},Node {prio = 10, key = 48, children = []}]},Node {prio = 9, key = 1, children = [Node {prio = 10, key = 2, children = []}]},Node {prio = 9, key = 0, children = []}]}]
            res10 = [EmptyTree,Node {prio = 2, key = 1, children = [Node {prio = 4, key = 8, children = []}]},Node {prio = 7, key = 12, children = [Node {prio = 9, key = 10, children = [Node {prio = 19, key = 29, children = []}]},Node {prio = 8, key = 11, children = []}]},Node {prio = 5, key = -3, children = [Node {prio = 18, key = 9, children = [Node {prio = 19, key = 11, children = [Node {prio = 20, key = 12, children = []}]},Node {prio = 19, key = 10, children = []}]},Node {prio = 6, key = -1, children = [Node {prio = 7, key = 0, children = []}]},Node {prio = 6, key = -2, children = []}]},Node {prio = 6, key = 100, children = [Node {prio = 10, key = 6, children = [Node {prio = 11, key = 10, children = [Node {prio = 12, key = 12, children = [Node {prio = 13, key = 13, children = []}]},Node {prio = 12, key = 11, children = []}]},Node {prio = 11, key = 8, children = [Node {prio = 12, key = 9, children = []}]},Node {prio = 11, key = 7, children = []}]},Node {prio = 7, key = 104, children = [Node {prio = 8, key = 106, children = [Node {prio = 9, key = 107, children = []}]},Node {prio = 8, key = 105, children = []}]},Node {prio = 7, key = 102, children = [Node {prio = 8, key = 103, children = []}]},Node {prio = 7, key = 101, children = []}]}]

testMerge :: TestData
testMerge = tests 8 10
    [
        testCond "merge 1" $ binomialHeapProperty expr1,
        testVal "merge 1 (exact)" res1 expr1,
        testCond "merge 2" $ binomialHeapProperty expr2,
        testVal "merge 2 (exact)" res2 expr2,
        testCond "merge 3" $ binomialHeapProperty expr3,
        testVal "merge 3 (exact)" res3 expr3,
        testCond "merge 4" $ binomialHeapProperty expr4,
        testVal "merge 4 (exact)" res4 expr4,
        testCond "merge 5" $ binomialHeapProperty expr5,
        testVal "merge 5 (exact)" res5 expr5
    ]
        where
            expr1 = merge (BinomialHeap 1 [Node 5 8 []]) (BinomialHeap 2 [EmptyTree, Node 5 9 [Node 13 11 []]])
            expr2 = merge (BinomialHeap 3 [Node 0 3 [], Node 4 11 [Node 5 19 []]]) (BinomialHeap 2 [EmptyTree, Node 98 24 [Node 102 118 []]])
            expr3 = merge (dummyHeap [0, 1, -1, 3, 4]) (dummyHeap [-1, 1, 2, -1, 4, 5])
            expr4 = merge (dummyHeap [0, -2, 2, 3, -1, -5, 6]) (dummyHeap [-1, 1, 2, -3, -1, 5, -6, 7])
            expr5 = merge (dummyHeap ([-1, -1] ++ [2..4])) (dummyHeap ([-1, -1, -1] ++ [3..5]))

            res1 = BinomialHeap {size = 3, trees = [Node {prio = 5, key = 8, children = []},Node {prio = 5, key = 9, children = [Node {prio = 13, key = 11, children = []}]}]}
            res2 = BinomialHeap {size = 5, trees = [Node {prio = 0, key = 3, children = []},EmptyTree,Node {prio = 4, key = 11, children = [Node {prio = 98, key = 24, children = [Node {prio = 102, key = 118, children = []}]},Node {prio = 5, key = 19, children = []}]}]}
            res3 = BinomialHeap {size = 81, trees = [Node {prio = 0, key = 0, children = []},EmptyTree,EmptyTree,EmptyTree,Node {prio = 0, key = 0, children = [Node {prio = 0, key = 0, children = [Node {prio = 1, key = 4, children = [Node {prio = 2, key = 6, children = [Node {prio = 3, key = 7, children = []}]},Node {prio = 2, key = 5, children = []}]},Node {prio = 1, key = 2, children = [Node {prio = 2, key = 3, children = []}]},Node {prio = 1, key = 1, children = []}]},Node {prio = 0, key = 0, children = [Node {prio = 1, key = 2, children = [Node {prio = 2, key = 3, children = []}]},Node {prio = 1, key = 1, children = []}]},Node {prio = 0, key = 0, children = [Node {prio = 1, key = 1, children = []}]},Node {prio = 1, key = 1, children = []}]},EmptyTree,Node {prio = 0, key = 0, children = [Node {prio = 0, key = 0, children = [Node {prio = 1, key = 16, children = [Node {prio = 2, key = 24, children = [Node {prio = 3, key = 28, children = [Node {prio = 4, key = 30, children = [Node {prio = 5, key = 31, children = []}]},Node {prio = 4, key = 29, children = []}]},Node {prio = 3, key = 26, children = [Node {prio = 4, key = 27, children = []}]},Node {prio = 3, key = 25, children = []}]},Node {prio = 2, key = 20, children = [Node {prio = 3, key = 22, children = [Node {prio = 4, key = 23, children = []}]},Node {prio = 3, key = 21, children = []}]},Node {prio = 2, key = 18, children = [Node {prio = 3, key = 19, children = []}]},Node {prio = 2, key = 17, children = []}]},Node {prio = 1, key = 8, children = [Node {prio = 2, key = 12, children = [Node {prio = 3, key = 14, children = [Node {prio = 4, key = 15, children = []}]},Node {prio = 3, key = 13, children = []}]},Node {prio = 2, key = 10, children = [Node {prio = 3, key = 11, children = []}]},Node {prio = 2, key = 9, children = []}]},Node {prio = 1, key = 4, children = [Node {prio = 2, key = 6, children = [Node {prio = 3, key = 7, children = []}]},Node {prio = 2, key = 5, children = []}]},Node {prio = 1, key = 2, children = [Node {prio = 2, key = 3, children = []}]},Node {prio = 1, key = 1, children = []}]},Node {prio = 0, key = 0, children = [Node {prio = 1, key = 8, children = [Node {prio = 2, key = 12, children = [Node {prio = 3, key = 14, children = [Node {prio = 4, key = 15, children = []}]},Node {prio = 3, key = 13, children = []}]},Node {prio = 2, key = 10, children = [Node {prio = 3, key = 11, children = []}]},Node {prio = 2, key = 9, children = []}]},Node {prio = 1, key = 4, children = [Node {prio = 2, key = 6, children = [Node {prio = 3, key = 7, children = []}]},Node {prio = 2, key = 5, children = []}]},Node {prio = 1, key = 2, children = [Node {prio = 2, key = 3, children = []}]},Node {prio = 1, key = 1, children = []}]},Node {prio = 1, key = 8, children = [Node {prio = 2, key = 12, children = [Node {prio = 3, key = 14, children = [Node {prio = 4, key = 15, children = []}]},Node {prio = 3, key = 13, children = []}]},Node {prio = 2, key = 10, children = [Node {prio = 3, key = 11, children = []}]},Node {prio = 2, key = 9, children = []}]},Node {prio = 1, key = 4, children = [Node {prio = 2, key = 6, children = [Node {prio = 3, key = 7, children = []}]},Node {prio = 2, key = 5, children = []}]},Node {prio = 1, key = 2, children = [Node {prio = 2, key = 3, children = []}]},Node {prio = 1, key = 1, children = []}]}]}
            res4 = BinomialHeap {size = 243, trees = [Node {prio = 0, key = 0, children = []},Node {prio = 0, key = 0, children = [Node {prio = 1, key = 1, children = []}]},EmptyTree,EmptyTree,Node {prio = 0, key = 0, children = [Node {prio = 0, key = 0, children = [Node {prio = 1, key = 4, children = [Node {prio = 2, key = 6, children = [Node {prio = 3, key = 7, children = []}]},Node {prio = 2, key = 5, children = []}]},Node {prio = 1, key = 2, children = [Node {prio = 2, key = 3, children = []}]},Node {prio = 1, key = 1, children = []}]},Node {prio = 0, key = 0, children = [Node {prio = 1, key = 2, children = [Node {prio = 2, key = 3, children = []}]},Node {prio = 1, key = 1, children = []}]},Node {prio = 1, key = 2, children = [Node {prio = 2, key = 3, children = []}]},Node {prio = 1, key = 1, children = []}]},Node {prio = 0, key = 0, children = [Node {prio = 1, key = 16, children = [Node {prio = 2, key = 24, children = [Node {prio = 3, key = 28, children = [Node {prio = 4, key = 30, children = [Node {prio = 5, key = 31, children = []}]},Node {prio = 4, key = 29, children = []}]},Node {prio = 3, key = 26, children = [Node {prio = 4, key = 27, children = []}]},Node {prio = 3, key = 25, children = []}]},Node {prio = 2, key = 20, children = [Node {prio = 3, key = 22, children = [Node {prio = 4, key = 23, children = []}]},Node {prio = 3, key = 21, children = []}]},Node {prio = 2, key = 18, children = [Node {prio = 3, key = 19, children = []}]},Node {prio = 2, key = 17, children = []}]},Node {prio = 1, key = 8, children = [Node {prio = 2, key = 12, children = [Node {prio = 3, key = 14, children = [Node {prio = 4, key = 15, children = []}]},Node {prio = 3, key = 13, children = []}]},Node {prio = 2, key = 10, children = [Node {prio = 3, key = 11, children = []}]},Node {prio = 2, key = 9, children = []}]},Node {prio = 1, key = 4, children = [Node {prio = 2, key = 6, children = [Node {prio = 3, key = 7, children = []}]},Node {prio = 2, key = 5, children = []}]},Node {prio = 1, key = 2, children = [Node {prio = 2, key = 3, children = []}]},Node {prio = 1, key = 1, children = []}]},Node {prio = 0, key = 0, children = [Node {prio = 1, key = 32, children = [Node {prio = 2, key = 48, children = [Node {prio = 3, key = 56, children = [Node {prio = 4, key = 60, children = [Node {prio = 5, key = 62, children = [Node {prio = 6, key = 63, children = []}]},Node {prio = 5, key = 61, children = []}]},Node {prio = 4, key = 58, children = [Node {prio = 5, key = 59, children = []}]},Node {prio = 4, key = 57, children = []}]},Node {prio = 3, key = 52, children = [Node {prio = 4, key = 54, children = [Node {prio = 5, key = 55, children = []}]},Node {prio = 4, key = 53, children = []}]},Node {prio = 3, key = 50, children = [Node {prio = 4, key = 51, children = []}]},Node {prio = 3, key = 49, children = []}]},Node {prio = 2, key = 40, children = [Node {prio = 3, key = 44, children = [Node {prio = 4, key = 46, children = [Node {prio = 5, key = 47, children = []}]},Node {prio = 4, key = 45, children = []}]},Node {prio = 3, key = 42, children = [Node {prio = 4, key = 43, children = []}]},Node {prio = 3, key = 41, children = []}]},Node {prio = 2, key = 36, children = [Node {prio = 3, key = 38, children = [Node {prio = 4, key = 39, children = []}]},Node {prio = 3, key = 37, children = []}]},Node {prio = 2, key = 34, children = [Node {prio = 3, key = 35, children = []}]},Node {prio = 2, key = 33, children = []}]},Node {prio = 1, key = 16, children = [Node {prio = 2, key = 24, children = [Node {prio = 3, key = 28, children = [Node {prio = 4, key = 30, children = [Node {prio = 5, key = 31, children = []}]},Node {prio = 4, key = 29, children = []}]},Node {prio = 3, key = 26, children = [Node {prio = 4, key = 27, children = []}]},Node {prio = 3, key = 25, children = []}]},Node {prio = 2, key = 20, children = [Node {prio = 3, key = 22, children = [Node {prio = 4, key = 23, children = []}]},Node {prio = 3, key = 21, children = []}]},Node {prio = 2, key = 18, children = [Node {prio = 3, key = 19, children = []}]},Node {prio = 2, key = 17, children = []}]},Node {prio = 1, key = 8, children = [Node {prio = 2, key = 12, children = [Node {prio = 3, key = 14, children = [Node {prio = 4, key = 15, children = []}]},Node {prio = 3, key = 13, children = []}]},Node {prio = 2, key = 10, children = [Node {prio = 3, key = 11, children = []}]},Node {prio = 2, key = 9, children = []}]},Node {prio = 1, key = 4, children = [Node {prio = 2, key = 6, children = [Node {prio = 3, key = 7, children = []}]},Node {prio = 2, key = 5, children = []}]},Node {prio = 1, key = 2, children = [Node {prio = 2, key = 3, children = []}]},Node {prio = 1, key = 1, children = []}]},Node {prio = 0, key = 0, children = [Node {prio = 1, key = 64, children = [Node {prio = 2, key = 96, children = [Node {prio = 3, key = 112, children = [Node {prio = 4, key = 120, children = [Node {prio = 5, key = 124, children = [Node {prio = 6, key = 126, children = [Node {prio = 7, key = 127, children = []}]},Node {prio = 6, key = 125, children = []}]},Node {prio = 5, key = 122, children = [Node {prio = 6, key = 123, children = []}]},Node {prio = 5, key = 121, children = []}]},Node {prio = 4, key = 116, children = [Node {prio = 5, key = 118, children = [Node {prio = 6, key = 119, children = []}]},Node {prio = 5, key = 117, children = []}]},Node {prio = 4, key = 114, children = [Node {prio = 5, key = 115, children = []}]},Node {prio = 4, key = 113, children = []}]},Node {prio = 3, key = 104, children = [Node {prio = 4, key = 108, children = [Node {prio = 5, key = 110, children = [Node {prio = 6, key = 111, children = []}]},Node {prio = 5, key = 109, children = []}]},Node {prio = 4, key = 106, children = [Node {prio = 5, key = 107, children = []}]},Node {prio = 4, key = 105, children = []}]},Node {prio = 3, key = 100, children = [Node {prio = 4, key = 102, children = [Node {prio = 5, key = 103, children = []}]},Node {prio = 4, key = 101, children = []}]},Node {prio = 3, key = 98, children = [Node {prio = 4, key = 99, children = []}]},Node {prio = 3, key = 97, children = []}]},Node {prio = 2, key = 80, children = [Node {prio = 3, key = 88, children = [Node {prio = 4, key = 92, children = [Node {prio = 5, key = 94, children = [Node {prio = 6, key = 95, children = []}]},Node {prio = 5, key = 93, children = []}]},Node {prio = 4, key = 90, children = [Node {prio = 5, key = 91, children = []}]},Node {prio = 4, key = 89, children = []}]},Node {prio = 3, key = 84, children = [Node {prio = 4, key = 86, children = [Node {prio = 5, key = 87, children = []}]},Node {prio = 4, key = 85, children = []}]},Node {prio = 3, key = 82, children = [Node {prio = 4, key = 83, children = []}]},Node {prio = 3, key = 81, children = []}]},Node {prio = 2, key = 72, children = [Node {prio = 3, key = 76, children = [Node {prio = 4, key = 78, children = [Node {prio = 5, key = 79, children = []}]},Node {prio = 4, key = 77, children = []}]},Node {prio = 3, key = 74, children = [Node {prio = 4, key = 75, children = []}]},Node {prio = 3, key = 73, children = []}]},Node {prio = 2, key = 68, children = [Node {prio = 3, key = 70, children = [Node {prio = 4, key = 71, children = []}]},Node {prio = 3, key = 69, children = []}]},Node {prio = 2, key = 66, children = [Node {prio = 3, key = 67, children = []}]},Node {prio = 2, key = 65, children = []}]},Node {prio = 1, key = 32, children = [Node {prio = 2, key = 48, children = [Node {prio = 3, key = 56, children = [Node {prio = 4, key = 60, children = [Node {prio = 5, key = 62, children = [Node {prio = 6, key = 63, children = []}]},Node {prio = 5, key = 61, children = []}]},Node {prio = 4, key = 58, children = [Node {prio = 5, key = 59, children = []}]},Node {prio = 4, key = 57, children = []}]},Node {prio = 3, key = 52, children = [Node {prio = 4, key = 54, children = [Node {prio = 5, key = 55, children = []}]},Node {prio = 4, key = 53, children = []}]},Node {prio = 3, key = 50, children = [Node {prio = 4, key = 51, children = []}]},Node {prio = 3, key = 49, children = []}]},Node {prio = 2, key = 40, children = [Node {prio = 3, key = 44, children = [Node {prio = 4, key = 46, children = [Node {prio = 5, key = 47, children = []}]},Node {prio = 4, key = 45, children = []}]},Node {prio = 3, key = 42, children = [Node {prio = 4, key = 43, children = []}]},Node {prio = 3, key = 41, children = []}]},Node {prio = 2, key = 36, children = [Node {prio = 3, key = 38, children = [Node {prio = 4, key = 39, children = []}]},Node {prio = 3, key = 37, children = []}]},Node {prio = 2, key = 34, children = [Node {prio = 3, key = 35, children = []}]},Node {prio = 2, key = 33, children = []}]},Node {prio = 1, key = 16, children = [Node {prio = 2, key = 24, children = [Node {prio = 3, key = 28, children = [Node {prio = 4, key = 30, children = [Node {prio = 5, key = 31, children = []}]},Node {prio = 4, key = 29, children = []}]},Node {prio = 3, key = 26, children = [Node {prio = 4, key = 27, children = []}]},Node {prio = 3, key = 25, children = []}]},Node {prio = 2, key = 20, children = [Node {prio = 3, key = 22, children = [Node {prio = 4, key = 23, children = []}]},Node {prio = 3, key = 21, children = []}]},Node {prio = 2, key = 18, children = [Node {prio = 3, key = 19, children = []}]},Node {prio = 2, key = 17, children = []}]},Node {prio = 1, key = 8, children = [Node {prio = 2, key = 12, children = [Node {prio = 3, key = 14, children = [Node {prio = 4, key = 15, children = []}]},Node {prio = 3, key = 13, children = []}]},Node {prio = 2, key = 10, children = [Node {prio = 3, key = 11, children = []}]},Node {prio = 2, key = 9, children = []}]},Node {prio = 1, key = 4, children = [Node {prio = 2, key = 6, children = [Node {prio = 3, key = 7, children = []}]},Node {prio = 2, key = 5, children = []}]},Node {prio = 1, key = 2, children = [Node {prio = 2, key = 3, children = []}]},Node {prio = 1, key = 1, children = []}]}]}
            res5 = BinomialHeap {size = 84, trees = [EmptyTree,EmptyTree,Node {prio = 0, key = 0, children = [Node {prio = 1, key = 2, children = [Node {prio = 2, key = 3, children = []}]},Node {prio = 1, key = 1, children = []}]},EmptyTree,Node {prio = 0, key = 0, children = [Node {prio = 0, key = 0, children = [Node {prio = 1, key = 4, children = [Node {prio = 2, key = 6, children = [Node {prio = 3, key = 7, children = []}]},Node {prio = 2, key = 5, children = []}]},Node {prio = 1, key = 2, children = [Node {prio = 2, key = 3, children = []}]},Node {prio = 1, key = 1, children = []}]},Node {prio = 1, key = 4, children = [Node {prio = 2, key = 6, children = [Node {prio = 3, key = 7, children = []}]},Node {prio = 2, key = 5, children = []}]},Node {prio = 1, key = 2, children = [Node {prio = 2, key = 3, children = []}]},Node {prio = 1, key = 1, children = []}]},EmptyTree,Node {prio = 0, key = 0, children = [Node {prio = 0, key = 0, children = [Node {prio = 1, key = 16, children = [Node {prio = 2, key = 24, children = [Node {prio = 3, key = 28, children = [Node {prio = 4, key = 30, children = [Node {prio = 5, key = 31, children = []}]},Node {prio = 4, key = 29, children = []}]},Node {prio = 3, key = 26, children = [Node {prio = 4, key = 27, children = []}]},Node {prio = 3, key = 25, children = []}]},Node {prio = 2, key = 20, children = [Node {prio = 3, key = 22, children = [Node {prio = 4, key = 23, children = []}]},Node {prio = 3, key = 21, children = []}]},Node {prio = 2, key = 18, children = [Node {prio = 3, key = 19, children = []}]},Node {prio = 2, key = 17, children = []}]},Node {prio = 1, key = 8, children = [Node {prio = 2, key = 12, children = [Node {prio = 3, key = 14, children = [Node {prio = 4, key = 15, children = []}]},Node {prio = 3, key = 13, children = []}]},Node {prio = 2, key = 10, children = [Node {prio = 3, key = 11, children = []}]},Node {prio = 2, key = 9, children = []}]},Node {prio = 1, key = 4, children = [Node {prio = 2, key = 6, children = [Node {prio = 3, key = 7, children = []}]},Node {prio = 2, key = 5, children = []}]},Node {prio = 1, key = 2, children = [Node {prio = 2, key = 3, children = []}]},Node {prio = 1, key = 1, children = []}]},Node {prio = 0, key = 0, children = [Node {prio = 1, key = 8, children = [Node {prio = 2, key = 12, children = [Node {prio = 3, key = 14, children = [Node {prio = 4, key = 15, children = []}]},Node {prio = 3, key = 13, children = []}]},Node {prio = 2, key = 10, children = [Node {prio = 3, key = 11, children = []}]},Node {prio = 2, key = 9, children = []}]},Node {prio = 1, key = 4, children = [Node {prio = 2, key = 6, children = [Node {prio = 3, key = 7, children = []}]},Node {prio = 2, key = 5, children = []}]},Node {prio = 1, key = 2, children = [Node {prio = 2, key = 3, children = []}]},Node {prio = 1, key = 1, children = []}]},Node {prio = 1, key = 8, children = [Node {prio = 2, key = 12, children = [Node {prio = 3, key = 14, children = [Node {prio = 4, key = 15, children = []}]},Node {prio = 3, key = 13, children = []}]},Node {prio = 2, key = 10, children = [Node {prio = 3, key = 11, children = []}]},Node {prio = 2, key = 9, children = []}]},Node {prio = 1, key = 4, children = [Node {prio = 2, key = 6, children = [Node {prio = 3, key = 7, children = []}]},Node {prio = 2, key = 5, children = []}]},Node {prio = 1, key = 2, children = [Node {prio = 2, key = 3, children = []}]},Node {prio = 1, key = 1, children = []}]}]}

main :: IO ()
main = vmCheck [ testAttach
               , testInsertTree
               , testEmptyHeap
               , testInsert
               , testFindMin
               , testMergeTrees
               , testMerge
               ]
