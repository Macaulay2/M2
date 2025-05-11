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

