-- -*- coding: utf-8 -*-


checkLU = method()
checkLU(List,Matrix,Matrix) := (P,L,U) -> (
     R := ring L;
     Q := id_(R^#P) _ P;
     Q*L*U)
checkLU(List,MutableMatrix,MutableMatrix) := (P,L,U) -> (
     R := ring L;
     Q := mutableMatrix(id_(R^#P) _ P);
     Q*L*U)
--checkLU Matrix := (M) -> norm (checkLU time LUdecomposition M - M)

permutationMatrix = (p) -> id_(ZZ^#p) _ p
checkLU Matrix := (M) -> (
    (P,L,U) := LUdecomposition M;
    P = permutationMatrix P;
    assert(M == P*L*U);
    )

testLUBoundaryCases = (kk) -> (
    -- this should be in e/unit-tests too?
    a0 := map(kk^0,kk^0,0);
    a1 := map(kk^0,kk^5,0);
    a2 := map(kk^5,kk^0,0);
    b0 := mutableMatrix a0;
    b1 := mutableMatrix a1;
    b2 := mutableMatrix a2;
    (P,L,U) := LUdecomposition a0;
    assert(#P == 0 
        and numrows L == 0 
        and numcols L == 0 
        and numrows U == 0 
        and numcols U == 0);
    (P,L,U) = LUdecomposition a1;
    assert(#P == 0 
        and numrows L == 0 
        and numcols L == 0 
        and numrows U == 0 
        and numcols U == 5);
    (P,L,U) = LUdecomposition a2;
    assert(#P == 5 
        and numrows L == 5 
        and numcols L == 0 
        and numrows U == 0 
        and numcols U == 0);
    -- same for mutable matrices
    (P,L,U) = LUdecomposition b0;
    assert(#P == 0 
        and numrows L == 0 
        and numcols L == 0 
        and numrows U == 0 
        and numcols U == 0);
    (P,L,U) = LUdecomposition b1;
    assert(#P == 0 
        and numrows L == 0 
        and numcols L == 0 
        and numrows U == 0 
        and numcols U == 5);
    (P,L,U) = LUdecomposition b2;
    assert(#P == 5 
        and numrows L == 5 
        and numcols L == 0 
        and numrows U == 0 
        and numcols U == 0);
    -- now LU of zero matrices
    -- Note that this is documented in 'viewHelp "LUdecomposition"'
    z0 := mutableMatrix(kk,2,4);
    (P,L,U) = LUdecomposition z0;
    assert((numrows L, numcols L) == (2,2));
    assert((numrows U, numcols U) == (2,4));
    z1 := mutableMatrix(kk,4,2);
    (P,L,U) = LUdecomposition z1;
    assert((numrows L, numcols L) == (4,2));
    assert((numrows U, numcols U) == (2,2));
    -- Now test boundary cases for the derivative routines:
    -- determinant:
    assert(det a0 == 1_kk);
    assert(det b0 == 1_kk);
    -- rank:
    assert(rank a0 == 0);
    assert(rank a1 == 0);
    assert(rank a2 == 0);
    assert(rank b0 == 0);
    assert(rank b1 == 0);
    assert(rank b2 == 0);
    -- transpose
    assert(transpose a1 == a2);
    assert(transpose a0 == a0);
    assert(transpose b1 == b2);
    assert(transpose b0 == b0);
    -- rank profile
    assert(rowRankProfile b0 == {});
    assert(rowRankProfile b1 == {});
    assert(rowRankProfile b2 == {});
    assert(columnRankProfile b0 == {});
    assert(columnRankProfile b1 == {});
    assert(columnRankProfile b2 == {});
    -- solve
    s2 := solve(b2, mutableMatrix(kk,5,2));
    assert(numrows s2 == 0);    
    assert(numcols s2 == 2);
    s2 = solve(b2, mutableMatrix(kk,5,0));
    assert(s2 == b0);
    -- inverse
    assert(inverse b0 == b0);
    -- nullSpace
    n0 := nullSpace b0;
    assert(n0 == b0);
    n1 := nullSpace b1;
    assert(rank n1 == 5);
    n2 := nullSpace b2;
    assert(n2 == b0);
    -- multiplication
    assert(a1 * a2 == a0);
    assert(b1 * b2 == b0);
    assert(b2 * b1 == mutableMatrix(kk,5,5));
    )    

testLUTemplate = (rng ) -> (
    epsilon := .5_rng ^ (-10 + precision rng);
    R := rng;
    M := mutableMatrix(R,10,10);
    fillMatrix M;
    (P,L,U) := LUdecomposition M;
    assert(norm(M - checkLU(P,L,U)) < epsilon);

    M = mutableMatrix(R,100,100);
    fillMatrix M;
    time (P,L,U) = LUdecomposition M;
    assert(norm(M - checkLU(P,L,U)) < epsilon);

    M = mutableMatrix(R,500,500);
    fillMatrix M;
    time (P,L,U) = LUdecomposition M;
    assert(norm(M - checkLU(P,L,U)) < epsilon);
    )

testLUoverRR = () -> (
    R := RR_53;
    testLUTemplate(R);
    )

testLUoverRRR = () -> (
    R := RR_200;
    testLUTemplate(R);
    )


testLUoverCCC = () -> (
    R := CC_200;
   testLUTemplate(R);
    )

