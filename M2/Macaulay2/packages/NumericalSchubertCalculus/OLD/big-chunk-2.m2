
restart
check "NumericalSchubertCalculus"

-- EXAMPLES (see TEST section for more simple examples)
restart
setRandomSeed 0
--debug 
needsPackage "NumericalSchubertCalculus";

root = playCheckers({2,1},{2,1},3,6)
time resolveNode(root, {({2},random(FFF^6,FFF^6)), ({1},random(FFF^6,FFF^6))})
peek root

DEBUG'LEVEL = 1 --check that DebugLevel = 1 should do the black box solving only at the leaves
n=7; K'n=FFF^n; -- takes about 10 minutes!
root = playCheckers({2,1,0},{2,1,0},3,n)
time resolveNode(root, {({2,1,0},random(K'n,K'n)),({2,1,0},random(K'n,K'n))})
peek root
printTree root

root = playCheckers({2,1}, {2}, 3,6)
time resolveNode(root, {({2},random(FFF^6,FFF^6)), ({2},random(FFF^6,FFF^6))})
peek root
printTree root

---- there is something wrong, this problem gives an error
root = playCheckers({3,2,2},{2}, 3,6)
peek root
resolveNode(root, {})


restart
setRandomSeed 0
debug needsPackage "LRcheckergame";

-- we test if the resolveNode function
-- can just solve the problem when 
-- the Schubert problem consist of two
-- complementary partitions only
root = playCheckers({3,3,1},{2},3,6)
resolveNode(root,{})
peek root

-- this problem should give empty solutions as
-- the two partition are not complementary
root = playCheckers({3,3,1}, {1,1}, 3,6)
resolveNode(root, {({2,1,0},random(FFF^6,FFF^6))})


------------------- WHAT IS THIS STUFF? CAN IT BE DELETED??? ---------------------------
