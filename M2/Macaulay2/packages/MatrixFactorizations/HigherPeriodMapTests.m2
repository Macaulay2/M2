-- The image of delta alone need not consist of cycles in a p-complex.
-- Boundaries and their homotopy witnesses use delta^(p-1).
TEST ///
scan({ZZ/7[x,y,z], ZZ/5[a,b,c,d]}, S -> (
    S.rootOfUnity = 2_S;
    C := ZZdfactorization flatten entries vars S;
    p := period C;
    E := Hom(C,C);
    assert((dd^E)^p == 0);
    assert((dd^E)^(p-1) != 0);
    scan({-2,-1,0,1,2,3,4}, n -> (
        setRandomSeed 0;
        f := randomFactorizationMap(C,C,
            Boundary => true, Degree => n, InternalDegree => 2*p);
        assert(f != 0);
        assert isHomogeneous f;
        assert isCommutative f;
        -- Recheck the cache using every cyclic square and the same root of unity.
        assert isWellDefined f;
        assert isNullHomotopic f;
        h := nullHomotopy f;
        assert(degree h === n+p-1);
        assert isWellDefined h;
        assert isNullHomotopyOf(h,f);
        assert(homomorphism homomorphism' f == f);
        ));
    Z := ZZdfactorization toList(p:S^1);
    assert not isNullHomotopic id_Z;
    h := nullHomotopy id_Z;
    assert isWellDefined h;
    assert(degree h === p-1);
    assert not isNullHomotopyOf(h,id_Z);
    ));
///

-- The square across the end of the period must not be skipped.
TEST ///
S = QQ[x];
C = ZZdfactorization hashTable {
    1 => map(S^1,S^1,0),
    2 => map(S^1,S^1,{{x}}, Degree => 1)
    };
f = map(C,C,hashTable {0 => id_(C_0)});
assert not isCommutative f
assert isWellDefined f
f.cache.isCommutative = true;
assert not isWellDefined f
///
