TEST ///
    -----------------------
    -- identity permutation
    -----------------------
    trimmedIdentity = permutation {1}
    extendedIdentity = permutation toList (1..8)
    extendedList = toList (1 .. #extendedIdentity)
    
    -- pattern avoidance
    assert(isVexillary trimmedIdentity)
    assert(isVexillary extendedIdentity)
    assert(isCartwrightSturmfels trimmedIdentity)
    assert(isCartwrightSturmfels extendedIdentity)
    assert(isCDG trimmedIdentity)
    assert(isCDG extendedIdentity)
///

TEST ///
    -----------------------
    -- longest permutation
    -----------------------
    p = permutation reverse toList (1..10)
    pList = toList p
    extendedP = permutation(pList | {max(pList)+1, max(pList)+2})

    -- pattern avoidance
    assert(isVexillary p)
    assert(isCartwrightSturmfels p)
    assert(isCDG p)
///

TEST ///
    --------------------
    -- General pattern avoidance
    --------------------
    assert(not avoidsPattern(permutation {2,3,7,1,5,8,4,6}, {1,4,3,2}))
    assert(avoidsPattern(permutation {1,4,6,2,3,7,5}, {1,4,3,2}))

    -- assert(not isPatternAvoiding(permutation {3,1,2},{3,1,2}));
    -- assert(not isPatternAvoiding(permutation {1,2,3,6,4,5}, {3,1,2}));
    -- assert(isPatternAvoiding(permutation {3,1,2},{2,3,1}));

    assert(not isVexillary(permutation {7,2,5,8,1,3,6,4}))
    assert(isVexillary(permutation {1,6,9,2,4,7,3,5,8}))
///