f = id_(ZZ^5)
v = {1,2,4,3,0}
w = {3,1,2,4,0}
assert(f_(v_w)==(f_v)_w)
assert(f_(v_w)==(f_v)*(f_w))

-----------------------------------------------------------------------------
-- Permutation class
-----------------------------------------------------------------------------
-- Should expression, string, TeX, and HTML outputs be tested?

-- valid permutations should be nonempty lists consisting of only all numbers 1..n
assert(isValidPermutation {1})
assert(isValidPermutation toList (1..8))
assert(isValidPermutation random toList (1..8))
assert(not isValidPermutation {})
assert(not isValidPermutation {0})
assert(not isValidPermutation toList (0..8))
assert(not isValidPermutation random toList (0..8))
assert(not isValidPermutation {1,1,2})

-----------------------
-- identity permutation
-----------------------
reducedIdentity = permutation {1}
expandedIdentity = permutation toList (1..8)
expandedList = toList (1 .. #expandedIdentity)

-- indexing into a permutation
assert(reducedIdentity_0 == 1)
assert(expandedIdentity_{0..2} == {1,2,3})
assert(expandedIdentity_{0,2,4,6} == {1,3,5,7})
assert(expandedIdentity_(0..2) == {1,2,3})
assert(expandedIdentity_(0,2,4,6) == {1,3,5,7})

-- expand and reduce are inverse processes
assert(reduce expandedIdentity == reducedIdentity)
assert(expand(reducedIdentity, #expandedIdentity) == expandedIdentity)
assert(reduce expand(reducedIdentity, #expandedIdentity) == reducedIdentity)
assert(expand(reduce expandedIdentity, #expandedIdentity) == expandedIdentity)

-- basic operations
-- equality
assert(reducedIdentity == expandedIdentity)
assert(reducedIdentity != permutation reverse expandedList)
assert(expandedIdentity != permutation reverse expandedList)
-- multiplication
assert(reducedIdentity*reducedIdentity == reducedIdentity)
assert(reducedIdentity*expandedIdentity == reducedIdentity)
assert(expandedIdentity*expandedIdentity == reducedIdentity)
assert(expandedIdentity*(permutation switch(0, 1, expandedList)) == permutation switch(0, 1, expandedList))
assert((permutation switch(0, 1, expandedList))*expandedIdentity == (permutation switch(0, 1, expandedList)))
-- powers
assert(reducedIdentity^2 == reducedIdentity)
assert(reducedIdentity^5 == reducedIdentity)
assert(reducedIdentity^(-1) == reducedIdentity)
assert(reducedIdentity^(-5) == reducedIdentity)
assert(reducedIdentity^0 == reducedIdentity)

-- matrix representation
assert(toMatrix reducedIdentity == id_(ZZ^1))
assert(toMatrix expandedIdentity == id_(ZZ^(#expandedIdentity)))

-- Group actions
assert(reducedIdentity * {1} == {1})
assert(reducedIdentity * {1,2,3} == {1,2,3})
assert(reducedIdentity * {3,1,2} == {3,1,2})
assert(expandedIdentity * toList(1 .. #expandedIdentity) == toList(1 .. #expandedIdentity))
assert(expandedIdentity * toList(1 .. #expandedIdentity+2) == toList(1 .. #expandedIdentity+2))
assert(expandedIdentity * switch(0, 1, toList(1 .. #expandedIdentity+2)) == switch(0, 1, toList(1 .. #expandedIdentity+2)))

assert(reducedIdentity * (toMatrix reducedIdentity) == toMatrix reducedIdentity)
assert(reducedIdentity * (toMatrix expand(reducedIdentity, 3)) == toMatrix expand(reducedIdentity, 3))
assert(reducedIdentity * (toMatrix permutation {3,1,2}) == toMatrix permutation {3,1,2})
assert(expandedIdentity * (toMatrix expand(expandedIdentity, #expandedIdentity+2)) == toMatrix expand(expandedIdentity, #expandedIdentity+2))
assert(expandedIdentity * (toMatrix permutation switch(0, 1, toList(1 .. #expandedIdentity+2))) == toMatrix permutation switch(0, 1, toList(1 .. #expandedIdentity+2)))

-- cycle decomposition
assert(cycleDecomposition reducedIdentity == {1:1})
assert(cycleDecomposition expandedIdentity == for i in 1 .. #expandedIdentity list 1:i)
assert(cycleType reducedIdentity == 1:1)
assert(cycleType expandedIdentity == (#expandedIdentity):1)

-- Ascents, descents, runs, exceedances, and records
assert(ascents reducedIdentity == {})
assert(ascents expandedIdentity == toList (1 .. #expandedIdentity-1))
assert(descents reducedIdentity == {})
assert(descents expandedIdentity == {})
assert(ascendingRuns reducedIdentity == {1:1})
assert(ascendingRuns expandedIdentity == {(1 .. #expandedIdentity)})
assert(descendingRuns reducedIdentity == {1:1})
assert(descendingRuns expandedIdentity == for i in (1 .. #expandedIdentity) list 1:i)
assert(exceedances reducedIdentity == {})
assert(exceedances expandedIdentity == {})
assert(exceedances(reducedIdentity, Weak=>true) == {1})
assert(exceedances(expandedIdentity, Weak=>true) == expandedList)
assert(saliances reducedIdentity == {last reducedIdentity})
assert(saliances expandedIdentity == {last expandedIdentity})
assert(records reducedIdentity == {1})
assert(records expandedIdentity == expandedList)

-- pattern avoidance
assert(isVexillary reducedIdentity)
assert(isVexillary expandedIdentity)
assert(isCartwrightSturmfels reducedIdentity)
assert(isCartwrightSturmfels expandedIdentity)
assert(isCDG reducedIdentity)
assert(isCDG expandedIdentity)

-- Foata's fundamental bijection
assert(foataBijection reducedIdentity == reducedIdentity)
assert(foataBijection expandedIdentity == expandedIdentity)

-- miscellaneous
assert(inverse reducedIdentity == reducedIdentity)
assert(inverse expandedIdentity == expandedIdentity)
assert(ord reducedIdentity == 1)
assert(ord expandedIdentity == 1)
assert(sign reducedIdentity == 1)
assert(sign expandedIdentity == 1)
assert(isEven reducedIdentity)
assert(isEven expandedIdentity)
assert(not isOdd reducedIdentity)
assert(not isOdd expandedIdentity)
assert(not isDerangement reducedIdentity)
assert(not isDerangement expandedIdentity)
assert(fixedPoints reducedIdentity == {1})
assert(fixedPoints expandedIdentity == expandedList)
assert(inversions reducedIdentity == {})
assert(inversions expandedIdentity == {})

-----------------------
-- longest permutation
-----------------------
p = permutation reverse toList (1..10)
pList = toList p
expandedP = permutation(pList | {max(pList)+1, max(pList)+2})

-- indexing into a permutation
assert(p_0 == 10)
assert(p_{0..2} == {10,9,8})
assert(p_{0,2,4,6,8} == {10,8,6,4,2})
assert(p_(0..2) == {10,9,8})
assert(p_(0,2,4,6,8) == {10,8,6,4,2})

-- expand and reduce are inverse processes
assert(reduce p == p)
assert(expand(p, #p+2) == expandedP)
assert(reduce expand(p, #p+2) == p)

-- basic operations
-- equality
assert(p == expandedP)
assert(reducedIdentity != permutation reverse expandedList)
-- multiplication
assert(reducedIdentity*reducedIdentity == reducedIdentity)
-- powers
assert(p^2 == reducedIdentity)
assert(p^5 == p)
assert(p^(-1) == p)
assert(p^(-5) == p)
assert(p^0 == reducedIdentity)

-- matrix representation
antiDiagIdentity = matrix for i in (0 ..< #p) list for j in (0 ..< #p) list if i+j == #p-1 then 1 else 0
assert(toMatrix p == antiDiagIdentity)

-- Group actions
assert(p * toList(1 .. #p) == pList)
assert(p * toList(1 .. #p+2) == toList expandedP)
assert(p * {5,4,3,2,1,10,9,8,7,6} == {6,7,8,9,10,1,2,3,4,5})

assert(p * (toMatrix p) == id_(ZZ^#p))
assert(p * (toMatrix permutation {6,7,8,9,10,1,2,3,4,5}) == matrix {{0,0,0,0,1,0,0,0,0,0},
                                                                    {0,0,0,1,0,0,0,0,0,0},
                                                                    {0,0,1,0,0,0,0,0,0,0},
                                                                    {0,1,0,0,0,0,0,0,0,0},
                                                                    {1,0,0,0,0,0,0,0,0,0},
                                                                    {0,0,0,0,0,0,0,0,0,1},
                                                                    {0,0,0,0,0,0,0,0,1,0},
                                                                    {0,0,0,0,0,0,0,1,0,0},
                                                                    {0,0,0,0,0,0,1,0,0,0},
                                                                    {0,0,0,0,0,1,0,0,0,0}})

-- cycle decomposition
assert(cycleDecomposition p == {(6,5), (7,4), (8,3), (9,2), (10,1)})
assert(cycleType p == (2,2,2,2,2))

-- Ascents, descents, runs, exceedances, and records
assert(ascents p == {})
assert(descents p == {1,2,3,4,5,6,7,8,9})
assert(ascendingRuns p == for i in reverse (1 .. #p) list 1:i)
assert(descendingRuns p == {reverse (1 .. #p)})
assert(exceedances p == {1,2,3,4,5})
assert(exceedances(p, Weak=>true) == {1,2,3,4,5})
assert(saliances p == toList(1 .. #p))
assert(records p == {1})

-- pattern avoidance
assert(isVexillary p)
assert(isCartwrightSturmfels p)
assert(isCDG p)

-- Foata's fundamental bijection
assert(foataBijection p == permutation {6,5,7,4,8,3,9,2,10,1})

-- miscellaneous
assert(inverse p == p)
assert(ord p == 2)
assert(sign p == -1)
assert(not isEven p)
assert(isOdd p)
assert(isDerangement p)
assert(fixedPoints p == {})
assert(sort inversions p == sort subsets(1 .. #p, 2))


-------
-- Misc
-------
p = permutation random toList (1..10)
assert((inverse p)*p == reducedIdentity)
assert(p*(inverse p) == reducedIdentity)
assert(ord p == ord inverse p)
assert(cycleType p == cycleType inverse p)
assert(sign p == sign inverse p)

-- some more pattern avoidance
assert(not avoidsPattern(permutation {2,3,7,1,5,8,4,6}, {1,4,3,2}))
assert(avoidsPattern(permutation {1,4,6,2,3,7,5}, {1,4,3,2}))
assert(not isVexillary(permutation {7,2,5,8,1,3,6,4}))
assert(isVexillary(permutation {1,6,9,2,4,7,3,5,8}))
