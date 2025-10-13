TEST ///
    f = id_(ZZ^5)
    v = {1,2,4,3,0}
    w = {3,1,2,4,0}
    assert(f_(v_w)==(f_v)_w)
    assert(f_(v_w)==(f_v)*(f_w))
///

TEST ///
    -----------------------
    -- identity permutation
    -----------------------
    trimmedIdentity = permutation {1}
    extendedIdentity = permutation toList (1..8)
    extendedList = toList (1 .. #extendedIdentity)

    -- indexing into a permutation
    assert(trimmedIdentity_0 == 1)
    assert(extendedIdentity_{0..2} == {1,2,3})
    assert(extendedIdentity_{0,2,4,6} == {1,3,5,7})
    assert(extendedIdentity_(0..2) == {1,2,3})
    assert(extendedIdentity_(0,2,4,6) == {1,3,5,7})
///

TEST ///
    -----------------------
    -- longest permutation
    -----------------------
    p = permutation reverse toList (1..10)
    pList = toList p
    extendedP = permutation(pList | {max(pList)+1, max(pList)+2})

    -- indexing into a permutation
    assert(p_1 == 10)
    assert(p_{0..2} == {10,9,8})
    assert(p_{0,2,4,6,8} == {10,8,6,4,2})
    assert(p_(0..2) == {10,9,8})
    assert(p_(0,2,4,6,8) == {10,8,6,4,2})
///

TEST ///
    -----------------------
    -- identity permutation
    -----------------------
    trimmedIdentity = permutation {1}
    extendedIdentity = permutation toList (1..8)
    extendedList = toList (1 .. #extendedIdentity)

    -- basic operations
    -- equality
    assert(trimmedIdentity == extendedIdentity)
    assert(trimmedIdentity != permutation reverse extendedList)
    assert(extendedIdentity != permutation reverse extendedList)
    -- multiplication
    assert(trimmedIdentity*trimmedIdentity == trimmedIdentity)
    assert(trimmedIdentity*extendedIdentity == trimmedIdentity)
    assert(extendedIdentity*extendedIdentity == trimmedIdentity)
    assert(extendedIdentity*(permutation switch(0, 1, extendedList)) == permutation switch(0, 1, extendedList))
    assert((permutation switch(0, 1, extendedList))*extendedIdentity == (permutation switch(0, 1, extendedList)))
    -- powers
    assert(trimmedIdentity^2 == trimmedIdentity)
    assert(trimmedIdentity^5 == trimmedIdentity)
    assert(trimmedIdentity^(-1) == trimmedIdentity)
    assert(trimmedIdentity^(-5) == trimmedIdentity)
    assert(trimmedIdentity^0 == trimmedIdentity)
///

TEST ///
    -----------------------
    -- identity permutation
    -----------------------
    trimmedIdentity = permutation {1}

    -----------------------
    -- longest permutation
    -----------------------
    p = permutation reverse toList (1..10)
    pList = toList p
    extendedP = permutation(pList | {max(pList)+1, max(pList)+2})

    -- basic operations
    -- equality
    assert(p == extendedP)
    -- assert(trimmedIdentity != permutation reverse extendedList)
    assert(trimmedIdentity != reverse extendedP)
    -- multiplication
    assert(trimmedIdentity*trimmedIdentity == trimmedIdentity)
    -- powers
    assert(p^2 == trimmedIdentity)
    assert(p^5 == p)
    assert(p^(-1) == p)
    assert(p^(-5) == p)
    assert(p^0 == trimmedIdentity)
///

TEST ///
    -----------------------
    -- identity permutation
    -----------------------
    trimmedIdentity = permutation {1}
    extendedIdentity = permutation toList (1..8)
    extendedList = toList (1 .. #extendedIdentity)
    
    -- Group actions
    assert(trimmedIdentity * {1} == {1})
    assert(trimmedIdentity * {1,2,3} == {1,2,3})
    assert(trimmedIdentity * {3,1,2} == {3,1,2})
    assert(extendedIdentity * toList(1 .. #extendedIdentity) == toList(1 .. #extendedIdentity))
    assert(extendedIdentity * toList(1 .. #extendedIdentity+2) == toList(1 .. #extendedIdentity+2))
    assert(extendedIdentity * switch(0, 1, toList(1 .. #extendedIdentity+2)) == switch(0, 1, toList(1 .. #extendedIdentity+2)))

    assert(trimmedIdentity * (matrix trimmedIdentity) == matrix trimmedIdentity)
    assert(trimmedIdentity * (matrix extend(trimmedIdentity, 3)) == matrix extend(trimmedIdentity, 3))
    assert(trimmedIdentity * (matrix permutation {3,1,2}) == matrix permutation {3,1,2})
    assert(extendedIdentity * (matrix extend(extendedIdentity, #extendedIdentity+2)) == matrix extend(extendedIdentity, #extendedIdentity+2))
    assert(extendedIdentity * (matrix permutation switch(0, 1, toList(1 .. #extendedIdentity+2))) == matrix permutation switch(0, 1, toList(1 .. #extendedIdentity+2)))
///

TEST ///
    -----------------------
    -- longest permutation
    -----------------------
    p = permutation reverse toList (1..10)
    pList = toList p
    extendedP = permutation(pList | {max(pList)+1, max(pList)+2})

    -- Group actions
    assert(p * toList(1 .. #p) == pList)
    assert(p * toList(1 .. #p+2) == toList extendedP)
    assert(p * {5,4,3,2,1,10,9,8,7,6} == {6,7,8,9,10,1,2,3,4,5})
    assert(p * (5,4,3,2,1,10,9,8,7,6) == {6,7,8,9,10,1,2,3,4,5})
    assert(p * [5,4,3,2,1,10,9,8,7,6] == {6,7,8,9,10,1,2,3,4,5})

    assert(p * (matrix p) == id_(ZZ^#p))
    assert(p * (matrix permutation {6,7,8,9,10,1,2,3,4,5}) == matrix {{0,0,0,0,1,0,0,0,0,0},
                                                                        {0,0,0,1,0,0,0,0,0,0},
                                                                        {0,0,1,0,0,0,0,0,0,0},
                                                                        {0,1,0,0,0,0,0,0,0,0},
                                                                        {1,0,0,0,0,0,0,0,0,0},
                                                                        {0,0,0,0,0,0,0,0,0,1},
                                                                        {0,0,0,0,0,0,0,0,1,0},
                                                                        {0,0,0,0,0,0,0,1,0,0},
                                                                        {0,0,0,0,0,0,1,0,0,0},
                                                                        {0,0,0,0,0,1,0,0,0,0}})
///

TEST ///
    -----------------------
    -- longest permutation
    -----------------------
    p = permutation reverse toList (1..10)
    pList = toList p
    extendedP = permutation(pList | {max(pList)+1, max(pList)+2})

    -- cycle decomposition
    assert(cycleDecomposition p == {(6,5), (7,4), (8,3), (9,2), (10,1)})
    assert(cycleType p == (2,2,2,2,2))
///

TEST ///
    -----------------------
    -- longest permutation
    -----------------------
    p = permutation reverse toList (1..10)
    pList = toList p
    extendedP = permutation(pList | {max(pList)+1, max(pList)+2})

    -- Ascents, descents, runs, exceedances, and records
    assert(ascents p == {})
    assert(descents p == {1,2,3,4,5,6,7,8,9})
    assert(ascendingRuns p == for i in reverse (1 .. #p) list 1:i)
    assert(descendingRuns p == {reverse (1 .. #p)})
    assert(exceedances p == {1,2,3,4,5})
    assert(exceedances(p, Weak=>true) == {1,2,3,4,5})
    assert(saliances p == toList(1 .. #p))
    assert(records p == {1})
///

TEST ///
    -----------------------
    -- longest permutation
    -----------------------
    p = permutation reverse toList (1..10)
    pList = toList p
    extendedP = permutation(pList | {max(pList)+1, max(pList)+2})

    -- Foata's fundamental bijection
    assert(foataBijection p == permutation {6,5,7,4,8,3,9,2,10,1})
///

TEST ///
    -----------------------
    -- longest permutation
    -----------------------
    p = permutation reverse toList (1..10)
    pList = toList p
    extendedP = permutation(pList | {max(pList)+1, max(pList)+2})

    -- miscellaneous
    assert(inverse p == p)
    assert(ord p == 2)
    assert(sign p == -1)
    assert(not isEven p)
    assert(isOdd p)
    assert(isDerangement p)
    assert(fixedPoints p == {})
    assert(sort inversions p == sort subsets(1 .. #p, 2))
    assert(length p == binomial(#(toList p), 2))
///

TEST ///
    -----------------------
    -- identity permutation
    -----------------------
    trimmedIdentity = permutation {1}

    -------
    -- Misc
    -------
    p = permutation random toList (1..10)
    assert((inverse p)*p == trimmedIdentity)
    assert(p*(inverse p) == trimmedIdentity)
    assert(ord p == ord inverse p)
    assert(cycleType p == cycleType inverse p)
    assert(sign p == sign inverse p)
///