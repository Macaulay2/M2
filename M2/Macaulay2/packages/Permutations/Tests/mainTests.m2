TEST ///
    -----------------------------------------------------------------------------
    -- Permutation class
    -----------------------------------------------------------------------------
    -- Should expression, string, TeX, and HTML outputs be tested?

    -- valid permutations should be nonempty lists consisting of only all numbers 1..n
    assert(isWellDefined permutation {1})
    assert(isWellDefined permutation  toList (1..8))
    assert(isWellDefined permutation random toList (1..8))
    assert(not isWellDefined permutation {})
    assert(not isWellDefined permutation {0})
    assert(not isWellDefined permutation toList (0..8))
    assert(not isWellDefined permutation random toList (0..8))
    assert(not isWellDefined permutation {1,1,2})
///

TEST ///
    -----------------------
    -- identity permutation
    -----------------------
    trimmedIdentity = permutation {1}
    extendedIdentity = permutation toList (1..8)
    extendedList = toList (1 .. #extendedIdentity)

    -- extend and trim are inverse processes
    assert(trim extendedIdentity == trimmedIdentity)
    assert(extend(trimmedIdentity, #extendedIdentity) == extendedIdentity)
    assert(trim extend(trimmedIdentity, #extendedIdentity) == trimmedIdentity)
    assert(extend(trim extendedIdentity, #extendedIdentity) == extendedIdentity)
///

TEST ///
    -----------------------
    -- longest permutation
    -----------------------
    p = permutation reverse toList (1..10)
    pList = toList p
    extendedP = permutation(pList | {max(pList)+1, max(pList)+2})

    -- extend and trim are inverse processes
    assert(trim p == p)
    assert(extend(p, #p+2) == extendedP)
    assert(trim extend(p, #p+2) == p)
///

TEST ///
    -----------------------
    -- identity permutation
    -----------------------
    trimmedIdentity = permutation {1}
    extendedIdentity = permutation toList (1..8)
    extendedList = toList (1 .. #extendedIdentity)
    
    -- matrix representation
    assert(matrix trimmedIdentity == id_(ZZ^1))
    assert(matrix extendedIdentity == id_(ZZ^(#extendedIdentity)))
///

TEST ///
    -----------------------
    -- longest permutation
    -----------------------
    p = permutation reverse toList (1..10)
    pList = toList p
    extendedP = permutation(pList | {max(pList)+1, max(pList)+2})

    -- matrix representation
    antiDiagIdentity = matrix for i in (0 ..< #p) list for j in (0 ..< #p) list if i+j == #p-1 then 1 else 0
    assert(matrix p == antiDiagIdentity)
///

TEST ///
    -- transpositions
    assert(transposition(1) == permutation {2,1})
    assert(transposition(5) == permutation {1,2,3,4,6,5})

    assert(transposition(1,2) == transposition(1))
    assert(transposition(5,6) == transposition(5))
    assert(transposition(3,5) == permutation {1,2,5,4,3})
///

TEST ///
    -----------------------
    -- identity permutation
    -----------------------
    trimmedIdentity = permutation {1}
    extendedIdentity = permutation toList (1..8)
    extendedList = toList (1 .. #extendedIdentity)
    
    -- cycle decomposition
    assert(cycleDecomposition trimmedIdentity == {1:1})
    assert(cycleDecomposition extendedIdentity == for i in 1 .. #extendedIdentity list 1:i)
    assert(cycleType trimmedIdentity == 1:1)
    assert(cycleType extendedIdentity == (#extendedIdentity):1)
///

TEST ///
    -----------------------
    -- identity permutation
    -----------------------
    trimmedIdentity = permutation {1}
    extendedIdentity = permutation toList (1..8)
    extendedList = toList (1 .. #extendedIdentity)
    
    -- Ascents, descents, runs, exceedances, and records
    assert(ascents trimmedIdentity == {})
    assert(ascents extendedIdentity == toList (1 .. #extendedIdentity-1))
    assert(descents trimmedIdentity == {})
    assert(descents extendedIdentity == {})
    assert(ascendingRuns trimmedIdentity == {1:1})
    assert(ascendingRuns extendedIdentity == {(1 .. #extendedIdentity)})
    assert(descendingRuns trimmedIdentity == {1:1})
    assert(descendingRuns extendedIdentity == for i in (1 .. #extendedIdentity) list 1:i)
    assert(exceedances trimmedIdentity == {})
    assert(exceedances extendedIdentity == {})
    assert(exceedances(trimmedIdentity, Weak=>true) == {1})
    assert(exceedances(extendedIdentity, Weak=>true) == extendedList)
    assert(saliances trimmedIdentity == {last trimmedIdentity})
    assert(saliances extendedIdentity == {last extendedIdentity})
    assert(records trimmedIdentity == {1})
    assert(records extendedIdentity == extendedList)
///

TEST ///
    -----------------------
    -- identity permutation
    -----------------------
    trimmedIdentity = permutation {1}
    extendedIdentity = permutation toList (1..8)
    extendedList = toList (1 .. #extendedIdentity)
    
    -- Foata's fundamental bijection
    assert(foataBijection trimmedIdentity == trimmedIdentity)
    assert(foataBijection extendedIdentity == extendedIdentity)
///

TEST ///
    -----------------------
    -- identity permutation
    -----------------------
    trimmedIdentity = permutation {1}
    extendedIdentity = permutation toList (1..8)
    extendedList = toList (1 .. #extendedIdentity)
    
    -- miscellaneous
    assert(inverse trimmedIdentity == trimmedIdentity)
    assert(inverse extendedIdentity == extendedIdentity)
    assert(ord trimmedIdentity == 1)
    assert(ord extendedIdentity == 1)
    assert(sign trimmedIdentity == 1)
    assert(sign extendedIdentity == 1)
    assert(isEven trimmedIdentity)
    assert(isEven extendedIdentity)
    assert(not isOdd trimmedIdentity)
    assert(not isOdd extendedIdentity)
    assert(not isDerangement trimmedIdentity)
    assert(not isDerangement extendedIdentity)
    assert(fixedPoints trimmedIdentity == {1})
    assert(fixedPoints extendedIdentity == extendedList)
    assert(inversions trimmedIdentity == {})
    assert(inversions extendedIdentity == {})
    assert(length trimmedIdentity == 0)
    assert(length extendedIdentity == 0)
///

TEST ///
    -- reducedWords
    p = permutation {4,3,2,1}
    actualReducedWords = {{1,2,1,3,2,1},
                          {1,2,3,1,2,1},
                          {1,2,3,2,1,2},
                          {1,3,2,1,3,2},
                          {1,3,2,3,1,2},
                          {2,1,2,3,2,1},
                          {2,1,3,2,1,3},
                          {2,1,3,2,3,1},
                          {2,3,1,2,1,3},
                          {2,3,1,2,3,1},
                          {2,3,2,1,2,3},
                          {3,1,2,1,3,2},
                          {3,1,2,3,1,2},
                          {3,2,1,2,3,2},
                          {3,2,1,3,2,3},
                          {3,2,3,1,2,3}}
    computedWords = reducedWords p
    -- Should be the same reduced words.
    assert((set actualReducedWords) === (set computedWords))
    -- All reduced words should be the length of the permutation (by definition).
    assert(all(computedWords, word -> #word == length p))
    -- All reduced words should multiply out to the permutation.
    assert(all(computedWords, word -> product reverse apply(word, transposition) == p))

    -- Specific example
    p = permutation {3,1,2,5,4}
    actualReducedWords = {{1,2,4},
                          {1,4,2},
                          {4,1,2}}
    computedWords = reducedWords p
    assert((set actualReducedWords) === (set computedWords))
    -- All reduced words should be the length of the permutation (by definition).
    assert(all(computedWords, word -> #word == length p))
    -- All reduced words should multiply out to the permutation.
    assert(all(computedWords, word -> product reverse apply(word, transposition) == p))
///

TEST ///
    -- strong Bruhat order
    assert(strongBruhatOrder(permutation {3,5,1,2,4}, permutation {4,5,1,2,3}) == true)

    -- For the example below, the two permutations are not comparable in the strong Bruhat order.
    p = permutation {6,9,4,2,8,7,5,3,1}
    q = permutation {3,6,8,4,7,5,9,1,2}
    assert(not strongBruhatOrder(p, q))
    assert(not strongBruhatOrder(q, p))

    -- All strong Bruhat relations in S_3
    p1 = permutation {1,2,3}
    p2 = permutation {1,3,2}
    p3 = permutation {2,1,3}
    p4 = permutation {2,3,1}
    p5 = permutation {3,1,2}
    p6 = permutation {3,2,1}
    assert(strongBruhatOrder(p1, p2) and (not strongBruhatOrder(p2, p1)))
    assert(strongBruhatOrder(p1, p3) and (not strongBruhatOrder(p3, p1)))
    assert(strongBruhatOrder(p1, p4) and (not strongBruhatOrder(p4, p1)))
    assert(strongBruhatOrder(p1, p5) and (not strongBruhatOrder(p5, p1)))
    assert(strongBruhatOrder(p1, p6) and (not strongBruhatOrder(p6, p1)))
    assert((not strongBruhatOrder(p2, p3)) and (not strongBruhatOrder(p3, p2)))
    assert(strongBruhatOrder(p2, p4) and (not strongBruhatOrder(p4, p2)))
    assert(strongBruhatOrder(p2, p5) and (not strongBruhatOrder(p5, p2)))
    assert(strongBruhatOrder(p2, p6) and (not strongBruhatOrder(p6, p2)))
    assert(strongBruhatOrder(p3, p4) and (not strongBruhatOrder(p4, p3)))
    assert(strongBruhatOrder(p3, p5) and (not strongBruhatOrder(p5, p3)))
    assert(strongBruhatOrder(p3, p6) and (not strongBruhatOrder(p6, p3)))
    assert((not strongBruhatOrder(p4, p5)) and (not strongBruhatOrder(p5, p4)))
    assert(strongBruhatOrder(p4, p6) and (not strongBruhatOrder(p6, p4)))
    assert(strongBruhatOrder(p5, p6) and (not strongBruhatOrder(p6, p5)))


///

TEST ///
    -- weak Bruhat order

    -- All (right) weak Bruhat relations in S_3
    p1 = permutation {1,2,3}
    p2 = permutation {1,3,2}
    p3 = permutation {2,1,3}
    p4 = permutation {2,3,1}
    p5 = permutation {3,1,2}
    p6 = permutation {3,2,1}
    assert(weakBruhatOrder(p1, p2) and (not weakBruhatOrder(p2, p1)))
    assert(weakBruhatOrder(p1, p2) and (not weakBruhatOrder(p2, p1)))
    assert(weakBruhatOrder(p1, p3) and (not weakBruhatOrder(p3, p1)))
    assert(weakBruhatOrder(p1, p4) and (not weakBruhatOrder(p4, p1)))
    assert(weakBruhatOrder(p1, p5) and (not weakBruhatOrder(p5, p1)))
    assert(weakBruhatOrder(p1, p6) and (not weakBruhatOrder(p6, p1)))
    assert((not weakBruhatOrder(p2, p3)) and (not weakBruhatOrder(p3, p2)))
    assert((not weakBruhatOrder(p2, p4)) and (not weakBruhatOrder(p4, p2)))
    assert(weakBruhatOrder(p2, p5) and (not weakBruhatOrder(p5, p2)))
    assert(weakBruhatOrder(p2, p6) and (not weakBruhatOrder(p6, p2)))
    assert(weakBruhatOrder(p3, p4) and (not weakBruhatOrder(p4, p3)))
    assert((not weakBruhatOrder(p3, p5)) and (not weakBruhatOrder(p5, p3)))
    assert(weakBruhatOrder(p3, p6) and (not weakBruhatOrder(p6, p3)))
    assert((not weakBruhatOrder(p4, p5)) and (not weakBruhatOrder(p5, p4)))
    assert(weakBruhatOrder(p4, p6) and (not weakBruhatOrder(p6, p4)))
    assert(weakBruhatOrder(p5, p6) and (not weakBruhatOrder(p6, p5)))
///

TEST ///
    -- symmetric group poset with Bruhat orders
    n = 4
    P = symmetricGroupPoset(n, weakBruhatOrder)

    -- Some facts about the rank generating function of the poset.
    polyCoeffs = apply(flatten entries (coefficients rankGeneratingFunction P)#1, k -> sub(k, ZZ))
    d = binomial(n,2) + 1
    assert(#polyCoeffs == d)
    -- FACT: The rank generating function should be symmetric.
    assert(all(apply(#polyCoeffs // 2, i -> polyCoeffs_i == polyCoeffs_(d-i-1))))
    -- FACT: The rank generating function should be unimodal.
    -- Strategy: Calculate consecutive, pairwise differences and see if sign 
    --           changed in difference (increasing to decreasing or vice versa).
    --           Sign can only change at most one time.
    firstDiffs = delete(0, apply(drop(polyCoeffs,-1), drop(polyCoeffs,1), difference))
    signFlips = apply(drop(firstDiffs,-1), drop(firstDiffs,1), (i, j) -> if i*j < 0 then 1 else 0)
    assert(sum signFlips <= 1)

    -- The poset is Sperner.
    assert(isSperner P)
///