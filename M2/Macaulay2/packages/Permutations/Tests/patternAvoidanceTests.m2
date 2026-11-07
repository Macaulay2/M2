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

    assert(not isVexillary(permutation {7,2,5,8,1,3,6,4}))
    assert(isVexillary(permutation {1,6,9,2,4,7,3,5,8}))
///

TEST ///
    --------------------
    -- isSeparable
    --------------------
    -- Example appears in "Distributions of statistics on separable permutations" (Chen-Kitaev-Zhang, 2024)
    p = permutation {1,4,3,2,5,9,7,8,6}
    assert(isSeparable p)
    assert(isSeparable reverse p)
    assert(isSeparable inverse p)

    -- Example appears in "Distributions of statistics on separable permutations" (Chen-Kitaev-Zhang, 2024)
    p = permutation {5,8,7,6,9,4,2,3,1}
    assert(isSeparable p)
    assert(isSeparable reverse p)
    assert(isSeparable inverse p)
///

TEST ///
    --------------------
    -- avoidsPatterns (avoidance of several patterns at once)
    --------------------
    -- avoidsPatterns(w, patterns) holds iff w avoids every pattern in the list
    w = permutation {2,3,7,1,5,8,4,6}
    assert(not avoidsPattern(w, {1,4,3,2}))
    assert(not avoidsPatterns(w, {{1,4,3,2}}))
    -- a single-pattern list agrees with avoidsPattern
    assert(avoidsPatterns(w, {{2,1}}) == avoidsPattern(w, {2,1}))
    -- the empty list of patterns is vacuously avoided
    assert(avoidsPatterns(w, {}))
    -- avoidsPatterns fails when ANY listed pattern is contained: {2,4,1,3}
    -- avoids {3,1,4,2} but contains {2,4,1,3}
    u = permutation {2,4,1,3}
    assert(avoidsPattern(u, {3,1,4,2}))
    assert(not avoidsPattern(u, {2,4,1,3}))
    assert(not avoidsPatterns(u, {{2,4,1,3}, {3,1,4,2}}))
    -- isSeparable is exactly avoidance of the patterns {2,4,1,3} and {3,1,4,2}
    p = permutation {1,4,3,2,5,9,7,8,6}
    assert(avoidsPatterns(p, {{2,4,1,3}, {3,1,4,2}}) == isSeparable p)
    assert(avoidsPatterns(u, {{2,4,1,3}, {3,1,4,2}}) == isSeparable u)
///